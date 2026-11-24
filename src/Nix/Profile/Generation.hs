{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A profile: a link to the newest of its generations, each a link to a
-- store path holding the profile's tree and its @manifest.json@, and each
-- a root the collector keeps, in upstream's layout (src\/libstore\/profiles.cc
-- at 2.35.4).
--
-- A user's profiles are in @profiles@ under the state directory:
-- @$NIX_STATE_HOME@, else @$XDG_STATE_HOME\/nix@, else
-- @~\/.local\/state\/nix@, and on Windows @\<Local AppData\>\\nix\\state@
-- (@getStateDir@ in users.cc, @getStateHome@ in xdg-dirs.cc).  The default
-- profile is @profiles\/profile@, which @~\/.nix-profile@, or
-- @\<state\>\/profile@ under @use-xdg-base-directories@, links to
-- (@getDefaultProfile@ in profiles.cc).  Upstream keeps root's profiles under
-- its state directory instead; here every user's are where a user's are.
--
-- A profile named @P@ has generations @P-1-link@, @P-2-link@ and on beside
-- it, and @P@ links to one of them by its bare name.  A new generation is
-- one past the highest, unless the highest already names the path, which
-- it then stays (@createGeneration@ in profiles.cc).  Each is linked as
-- @nova-nix build@ links its output ('Nix.Store.Ops.soAddOutLinkRoot',
-- upstream's @addPermRoot@), so it is an indirect root of the store it names.
--
-- A change to a profile reads the profile's link, makes the new
-- generation's store path with no lock held, then takes the profile's
-- lock and starts again if the link moved meanwhile, as @nix-env@ does
-- (@createUserEnv@ in user-env.cc).  Upstream's @nix profile@ takes no lock,
-- so two changes at once can both start from one manifest and the later
-- switch drop what the earlier added.  The lock file stays once it is
-- released, as every lock file nova-nix takes does ("Nix.Store.Lock"), where
-- upstream deletes it.
--
-- The location, the names and the decisions are pure; the IO below lists
-- the profile's directory, reads its links, takes its lock and makes its
-- links.
module Nix.Profile.Generation
  ( -- * Where profiles are
    StateHome (..),
    ProfileEnvironment (..),
    ProfileLinkPlace (..),
    stateDirectory,
    profilesDirectory,
    profileLinkPath,
    defaultProfilePath,
    absolutePath,
    readProfileEnvironment,
    resolveDefaultProfile,

    -- * Generations
    GenerationNumber (..),
    Generation (..),
    generationName,
    parseGenerationName,
    generationsIn,
    NextGeneration (..),
    nextGeneration,
    listGenerations,

    -- * A generation's store path
    NarTargets (..),
    hostNarTargets,
    narLinkTarget,
    generationNar,
    generationStoreName,

    -- * Changing a profile
    ProfileFailure (..),
    Change (..),
    Updated (..),
    readManifest,
    checkProfileStore,
    updateProfile,
  )
where

import Control.Exception (IOException, bracket, mask_, onException, try)
import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, sort, sortOn, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import Nix.Environment (EnvLookup (..), lookupEnvBytes)
import Nix.HostPath (hostPathBytes, hostPathFromBytes, hostPathText)
import Nix.Profile.Manifest (Conflict, Manifest, ManifestError, attributeConflict, emptyManifest, manifestPackages, manifestReferences, parseManifest, renderManifest)
import Nix.Profile.Tree (BuildEnvFailure (..), Built (..), Node (..), Package (..), Tree, buildProfile, storeTreeReader, warningText)
import Nix.Store.Handle (canonicalStoreDir)
import Nix.Store.Lock (LockMode (..), acquireLockFileWith, lockFileSuffix, releasePathLock)
import Nix.Store.Ops (StoreOps (..))
import Nix.Store.Path (StoreDir (..), StorePath, parseStorePathPrefix, storePathToFilePath)
import qualified NovaCache.NAR as NAR
import qualified System.Directory.OsPath as OsDir
import System.IO (stderr)
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import qualified System.Info
import System.OsPath (OsPath, (</>))
import qualified System.OsPath as OP
import Text.Read (readMaybe)
#if defined(mingw32_HOST_OS)
import Data.Maybe (fromMaybe)
import Foreign.Ptr (nullPtr)
import System.Win32.Shell (cSIDL_LOCAL_APPDATA, sHGFP_TYPE_CURRENT, sHGetFolderPath)
#else
import Foreign.C.Error (Errno (..), eNOTDIR)
import GHC.IO.Exception (IOException (ioe_errno))
import System.Posix.Files.ByteString (fileOwner, getFileStatus)
import System.Posix.User (getEffectiveUserID)
import System.Posix.User.ByteString (getUserEntryForID, homeDirectory)
#endif

-- ---------------------------------------------------------------------------
-- Where profiles are
-- ---------------------------------------------------------------------------

-- | What names a user's state directory.
data StateHome
  = -- | @NIX_STATE_HOME@, which names it.
    NixStateHome !OsPath
  | -- | On Unix, when @NIX_STATE_HOME@ is unset: @XDG_STATE_HOME@, when
    -- that is set, under which it is @nix@.
    XdgStateHome !(Maybe OsPath)
  | -- | On Windows, when @NIX_STATE_HOME@ is unset: the Local AppData
    -- folder, under which it is @nix\\state@.
    LocalAppDataHome !OsPath
  deriving (Eq, Show)

-- | What decides where a user's profiles are.
data ProfileEnvironment = ProfileEnvironment
  { peStateHome :: !StateHome,
    -- | The user's home: upstream's @getHome@, @HOME@ on Unix and
    -- @USERPROFILE@ on Windows.
    peHome :: !OsPath
  }
  deriving (Eq, Show)

-- | Where the link to the default profile is, as
-- @use-xdg-base-directories@ chooses (@useXDGBaseDirectories@ in
-- globals.hh, @getDefaultProfile@ in profiles.cc).
data ProfileLinkPlace
  = -- | @~\/.nix-profile@, upstream's default.
    HomeProfileLink
  | -- | @\<state\>\/profile@.
    StateProfileLink
  deriving (Eq, Show)

-- | The state directory (@getStateDir@ in users.cc).  A variable is used
-- as it is set, an empty one included, as upstream's @getEnv@ reads it.
stateDirectory :: ProfileEnvironment -> OsPath
stateDirectory env = case peStateHome env of
  NixStateHome dir -> dir
  XdgStateHome (Just dir) -> dir </> ascii "nix"
  XdgStateHome Nothing -> peHome env </> ascii ".local" </> ascii "state" </> ascii "nix"
  LocalAppDataHome dir -> dir </> ascii "nix" </> ascii "state"

-- | The directory a user's profiles are in (@profilesDir@ in profiles.cc).
profilesDirectory :: ProfileEnvironment -> OsPath
profilesDirectory env = stateDirectory env </> ascii "profiles"

-- | The link to the default profile.
profileLinkPath :: ProfileLinkPlace -> ProfileEnvironment -> OsPath
profileLinkPath place env = case place of
  HomeProfileLink -> peHome env </> ascii ".nix-profile"
  StateProfileLink -> stateDirectory env </> ascii "profile"

-- | The default profile the link is made to: @profiles\/profile@.
defaultProfilePath :: ProfileEnvironment -> OsPath
defaultProfilePath env = profilesDirectory env </> ascii "profile"

-- | A path made absolute against a directory and collapsed, @.@ dropped
-- and @..@ taking off what precedes it, without reading a link:
-- upstream's @absPath@, which @--profile@ and a link's target go through
-- (@MixProfile::updateProfile@ in command.cc, @getDefaultProfile@ in
-- profiles.cc).
absolutePath :: OsPath -> OsPath -> OsPath
absolutePath base path = OP.joinPath (reverse (foldl' step [] (OP.splitDirectories joined)))
  where
    joined
      | OP.isAbsolute path = path
      | otherwise = base </> path
    -- The components kept so far, last first.  A @..@ at the root stays
    -- at the root.
    step kept component
      | component == ascii "." = kept
      | component == ascii "..", parent : rest <- kept, not (OP.isAbsolute parent), parent /= component = rest
      | component == ascii "..", root : _ <- kept, OP.isAbsolute root = kept
      | otherwise = component : kept

-- | Read where this user's profiles are from the environment, as upstream
-- does: @NIX_STATE_HOME@, then on Unix @XDG_STATE_HOME@ and on Windows
-- the Local AppData known folder, which is asked only when
-- @NIX_STATE_HOME@ is unset; and the home.
readProfileEnvironment :: IO (Either Text ProfileEnvironment)
readProfileEnvironment = runExceptT $ do
  nixStateHome <- ExceptT (variablePath "NIX_STATE_HOME")
  stateHome <- case nixStateHome of
    Just dir -> pure (NixStateHome dir)
    Nothing -> ExceptT hostStateHome
  ProfileEnvironment stateHome <$> ExceptT readHome

-- | A variable's value as a host path, 'Nothing' when it is unset.
variablePath :: ByteString -> IO (Either Text (Maybe OsPath))
variablePath name = do
  found <- lookupEnvBytes name
  pure $ case found of
    EnvUnset -> Right Nothing
    EnvValue bytes -> maybe (Left (variableName <> " is not valid UTF-8, so it names no Windows path")) (Right . Just) (hostPathFromBytes bytes)
    EnvUnpairedSurrogate -> Left (variableName <> " holds an unpaired UTF-16 surrogate, which has no UTF-8 form")
  where
    variableName = T.pack (BS8.unpack name)

-- | What names the state directory on this host when @NIX_STATE_HOME@
-- does not.
hostStateHome :: IO (Either Text StateHome)
#if defined(mingw32_HOST_OS)
hostStateHome = do
  -- SHGetFolderPath's CSIDL_LOCAL_APPDATA is the folder
  -- SHGetKnownFolderPath's FOLDERID_LocalAppData names, which upstream
  -- asks (getLocalAppData in windows/known-folders.cc); neither reads
  -- LOCALAPPDATA.
  found <- try (sHGetFolderPath nullPtr cSIDL_LOCAL_APPDATA nullPtr sHGFP_TYPE_CURRENT)
  case found of
    Left (failure :: IOException) -> pure (Left ("failed to get known folder path: " <> T.pack (show failure)))
    Right dir -> Right . LocalAppDataHome <$> OP.encodeFS dir
#else
hostStateHome = fmap XdgStateHome <$> variablePath "XDG_STATE_HOME"
#endif

-- | The user's home, as upstream's @getHome@ finds it.
readHome :: IO (Either Text OsPath)
#if defined(mingw32_HOST_OS)
-- @USERPROFILE@, else the default user's profile, collapsed
-- (@getHome@ in windows/users.cc).
readHome = do
  found <- variablePath "USERPROFILE"
  cwd <- OsDir.getCurrentDirectory
  pure (absolutePath cwd . fromMaybe defaultUserProfile <$> found)
  where
    defaultUserProfile = ascii "C:\\Users\\Default"
#else
-- @HOME@, unless what it names exists and another user owns it, or its
-- status cannot be read for a reason other than its absence, which
-- @ENOENT@ and @ENOTDIR@ are (@maybeStat@ in file-system.cc), when the
-- password database's home is used, with upstream's warning (@getHome@ in
-- unix/users.cc).
readHome = do
  found <- lookupEnvBytes "HOME"
  case found of
    EnvValue bytes -> do
      probed <- try (getFileStatus bytes)
      user <- getEffectiveUserID
      case probed of
        Right status
          | fileOwner status /= user -> passwordHome (Just bytes)
          | otherwise -> pure (homePath bytes)
        Left (failure :: IOException)
          | isDoesNotExistError failure || fmap Errno (ioe_errno failure) == Just eNOTDIR -> pure (homePath bytes)
          | otherwise -> do
              TIO.hPutStrLn stderr ("warning: couldn't stat $HOME ('" <> shown bytes <> "') for reason other than not existing, falling back to the one defined in the 'passwd' file: " <> T.pack (show failure))
              passwordHome Nothing
    _ -> passwordHome Nothing
  where
    homePath bytes = maybe (Left "HOME names no path") Right (hostPathFromBytes bytes)
    shown = T.pack . BS8.unpack
    passwordHome unowned = do
      entry <- tryIO (getUserEntryForID =<< getEffectiveUserID)
      case entry of
        Right found
          | not (BS.null (homeDirectory found)) -> do
              let home = homeDirectory found
              case unowned of
                Just given
                  | given /= home ->
                      TIO.hPutStrLn stderr ("warning: $HOME ('" <> shown given <> "') is not owned by you, falling back to the one defined in the 'passwd' file ('" <> shown home <> "')")
                _ -> pure ()
              pure (homePath home)
        _ -> pure (Left "cannot determine user's home directory")
#endif

-- | Resolve the default profile, making its directory and its link when
-- they are absent, as upstream's @getDefaultProfile@ does
-- (profiles.cc): the link's target, made absolute against the link's
-- directory, or the link itself when the directory or the link cannot be made
-- or the link cannot be read.  Absence is by @lstat@ (@pathExists@ in
-- file-system.cc), so a link that leads nowhere is kept.  Under
-- 'StateProfileLink' the state directory is made first, and a failure to make
-- it is the result.
resolveDefaultProfile :: ProfileLinkPlace -> ProfileEnvironment -> IO (Either Text OsPath)
resolveDefaultProfile place env = do
  made <- case place of
    StateProfileLink -> first creating <$> tryIO (OsDir.createDirectoryIfMissing True (stateDirectory env))
    HomeProfileLink -> pure (Right ())
  case made of
    Left failure -> pure (Left failure)
    Right () -> do
      resolved <- try $ do
        OsDir.createDirectoryIfMissing True (profilesDirectory env)
        absent <- linkAbsent link
        linked <- if absent then replaceLink link (defaultProfilePath env) else pure (Right ())
        case linked of
          Left _ -> pure link
          Right () -> absolutePath (OP.takeDirectory link) <$> OsDir.getSymbolicLinkTarget link
      pure (Right (either (\(_ :: IOException) -> link) id resolved))
  where
    link = profileLinkPath place env
    creating failure = "creating directory \"" <> hostPathText (stateDirectory env) <> "\": " <> T.pack (show failure)

-- ---------------------------------------------------------------------------
-- Generations
-- ---------------------------------------------------------------------------

-- | A generation's number (upstream's @GenerationNumber@, a 64-bit
-- unsigned integer).
newtype GenerationNumber = GenerationNumber Word64
  deriving (Eq, Ord, Show)

-- | A generation: its number and its link's path.
data Generation = Generation
  { generationNumber :: !GenerationNumber,
    generationPath :: !OsPath
  }
  deriving (Eq, Show)

-- | The link of a profile's generation: the profile's path with
-- @-N-link@ appended (@makeName@ in profiles.cc).
generationName :: OsPath -> GenerationNumber -> OsPath
generationName profile (GenerationNumber n) = profile <> ascii ("-" <> show n <> "-link")

-- | The generation a name in the profile's directory is, given the
-- profile's own name, as upstream's @parseName@ reads one
-- (profiles.cc): the profile's name and @-@, then what precedes the
-- first @-link@ after them, read as a 64-bit unsigned number is by
-- @string2Int@ in util.cc, which takes digits after an optional
-- @+@.  Whatever follows the first @-link@ is ignored, so
-- @profile-3-link.tmp@ is generation 3 too.
parseGenerationName :: OsPath -> OsPath -> Maybe GenerationNumber
parseGenerationName profileName name = do
  rest <- stripPrefix (OP.unpack profileName ++ [OP.unsafeFromChar '-']) (OP.unpack name)
  let chars = map OP.toChar rest
  number <- listToMaybe [take n chars | n <- [0 .. length chars], linkSuffix `isPrefixOf` drop n chars]
  GenerationNumber <$> readUnsigned number
  where
    linkSuffix = "-link"
    readUnsigned digits = case digits of
      '+' : rest -> unsigned rest
      _ -> unsigned digits
    unsigned digits
      | not (null digits), all isDigit digits = readMaybe digits >>= bounded
      | otherwise = Nothing
    bounded :: Integer -> Maybe Word64
    bounded n
      | n <= toInteger (maxBound :: Word64) = Just (fromInteger n)
      | otherwise = Nothing

-- | The generations of a profile among the names of its directory, in
-- order of number (@findGenerations@ in profiles.cc).  Names are taken in
-- sorted order, so of two names for one number, as @profile-3-link@ and
-- @profile-3-link.tmp@ are, the later in that order counts as the later
-- generation; upstream takes them in @readdir@ order.
generationsIn :: OsPath -> [OsPath] -> [Generation]
generationsIn profile names =
  sortOn generationNumber [Generation number (OP.takeDirectory profile </> name) | name <- sort names, Just number <- [parseGenerationName (OP.takeFileName profile) name]]

-- | What a new generation for a store path is.
data NextGeneration
  = -- | The newest generation, which already names the path.
    ReuseGeneration !Generation
  | -- | A new one, by number.
    NewGeneration !GenerationNumber
  deriving (Eq, Show)

-- | The generation a profile switches to for a store path, given its
-- newest generation with that generation's raw target, and the path as
-- the store spells it on disk (@createGeneration@ in profiles.cc).
nextGeneration :: Maybe (Generation, OsPath) -> OsPath -> NextGeneration
nextGeneration newest target = case newest of
  Just (generation, linked)
    | linked == target -> ReuseGeneration generation
    | GenerationNumber n <- generationNumber generation -> NewGeneration (GenerationNumber (n + 1))
  Nothing -> NewGeneration (GenerationNumber 1)

-- | A profile's generations, from a listing of its directory.
listGenerations :: OsPath -> IO (Either ProfileFailure [Generation])
listGenerations profile = do
  listed <- try (OsDir.listDirectory dir)
  pure $ case listed of
    Left (failure :: IOException) -> Left (ProfileIOFailed ("cannot read directory \"" <> hostPathText dir <> "\": " <> T.pack (show failure)))
    Right names -> Right (generationsIn profile names)
  where
    dir = OP.takeDirectory profile

-- ---------------------------------------------------------------------------
-- A generation's store path
-- ---------------------------------------------------------------------------

-- | The name a generation's store path has (@ProfileManifest::build@ in
-- profile.cc).
generationStoreName :: Text
generationStoreName = "profile"

-- | The file a generation's manifest is in (@ProfileManifest::build@ in
-- profile.cc).
manifestFileName :: String
manifestFileName = "manifest.json"

-- | The file a profile @nix-env@ made holds its manifest in
-- (the @ProfileManifest@ constructor in profile.cc).
nixEnvManifestFileName :: String
nixEnvManifestFileName = "manifest.nix"

-- | How a host's link targets are written in a NAR.
data NarTargets
  = -- | As their bytes, on every host but Windows.
    TargetBytes
  | -- | With @/@ for each @\\@, on Windows: a reparse point holds
    -- backslashes, and nova-cache's serialiser writes the target it reads
    -- from disk with slashes (@symlinkTargetBytes@), so a NAR is
    -- host-independent.
    SlashedTargets
  deriving (Eq, Show)

-- | How this host's link targets are written in a NAR.
hostNarTargets :: NarTargets
hostNarTargets
  | onWindows = SlashedTargets
  | otherwise = TargetBytes

-- | A link target's bytes as a NAR holds them.  A backslash is one byte,
-- @0x5C@, which no multi-byte UTF-8 sequence contains, so the replacement
-- is exact on a target's UTF-8.
narLinkTarget :: NarTargets -> ByteString -> ByteString
narLinkTarget targets bytes = case targets of
  TargetBytes -> bytes
  SlashedTargets -> BS8.map (\c -> if c == '\\' then '/' else c) bytes

-- | The NAR of a generation: the tree, and @manifest.json@ holding the
-- manifest's bytes, as upstream dumps the directory it makes
-- (@ProfileManifest::build@ in profile.cc).  A name and a link target are the
-- bytes the tree holds for them ('hostPathBytes'), a target written as the
-- host's serialiser writes it ('narLinkTarget'), so the NAR is the one the
-- tree unpacked from it serialises to.  'Left' for a name with no UTF-8 form.
generationNar :: NarTargets -> Tree -> ByteString -> Either Text NAR.NarEntry
generationNar targets tree manifest = NAR.NarDirectory . ((BS8.pack manifestFileName, NAR.NarRegular False manifest) :) <$> entries tree
  where
    entries sub = traverse entry (Map.toList sub)
    entry (name, node) = (,) <$> bytesOf name <*> nodeEntry node
    nodeEntry node = case node of
      Link target _ -> NAR.NarSymlink . narLinkTarget targets <$> bytesOf target
      Directory sub -> NAR.NarDirectory <$> entries sub
    bytesOf path = maybe (Left ("'" <> hostPathText path <> "' has no UTF-8 form")) Right (hostPathBytes path)

-- ---------------------------------------------------------------------------
-- Changing a profile
-- ---------------------------------------------------------------------------

-- | Why a profile could not be read or changed.
data ProfileFailure
  = -- | A file of the profile's could not be read or made, and why.
    ProfileIOFailed !Text
  | -- | The manifest, at this path, could not be read.
    ManifestFailed !OsPath !ManifestError
  | -- | A profile @nix-env@ made: it holds @manifest.nix@ and no
    -- @manifest.json@, and @nix-env@'s manifest is not read here.
    NixEnvProfile !OsPath
  | -- | A profile whose current generation is outside the opened store:
    -- the profile, and the path its links lead to.
    ForeignGeneration !OsPath !OsPath
  | -- | The tree could not be built.
    TreeFailed !BuildEnvFailure
  | -- | Two files of one priority conflict, by the elements they came from.
    TreeConflict !Conflict
  | -- | The store refused the generation's path or its root.
    StoreRefused !Text
  deriving (Eq, Show)

-- | What a change makes of a profile's manifest.
data Change
  = -- | Nothing: the profile keeps its generation.
    Unchanged
  | -- | The manifest of the profile's next generation.
    Changed !Manifest
  deriving (Eq, Show)

-- | What a change to a profile did.
data Updated
  = -- | Nothing: the change made none.
    NothingChanged
  | -- | The profile links to this generation.
    SwitchedTo !Generation
  deriving (Eq, Show)

-- | How one attempt at a change ends.
data Attempt = Restart | Finished !Updated

-- | Read a profile's manifest (the @ProfileManifest@ constructor in
-- profile.cc): @manifest.json@ through the profile's links, none for a
-- profile that is not there or holds neither manifest, and a refusal for one
-- only @nix-env@'s @manifest.nix@ describes.
readManifest :: OsPath -> IO (Either ProfileFailure Manifest)
readManifest profile = do
  contents <- try (OP.decodeFS path >>= BS.readFile)
  case contents of
    Right bytes -> pure (first (ManifestFailed path) (parseManifest bytes))
    Left (failure :: IOException)
      | isDoesNotExistError failure -> do
          nixEnv <- OsDir.doesPathExist (profile </> ascii nixEnvManifestFileName)
          pure (if nixEnv then Left (NixEnvProfile profile) else Right emptyManifest)
      | otherwise -> pure (Left (ProfileIOFailed ("reading file \"" <> hostPathText path <> "\": " <> T.pack (show failure))))
  where
    path = profile </> ascii manifestFileName

-- | Change a profile: read its link and its manifest, apply the change,
-- make the new generation's store path, then under the profile's lock
-- make and switch to the generation, starting again, with upstream's
-- line, when the link moved since it was read (@createUserEnv@ in
-- user-env.cc).  A profile whose current generation lies outside the store
-- ('checkProfileStore') is refused before anything is made: a generation of
-- another store read against this one would name paths it does not hold.  The
-- store directory is taken in the spelling the store's handle keys it by
-- ('canonicalStoreDir'), which the tree's links and the generations' targets
-- are written in.
updateProfile :: StoreOps IO -> StoreDir -> OsPath -> (Manifest -> IO Change) -> IO (Either ProfileFailure Updated)
updateProfile ops givenStoreDir profile change = canonicalStoreDir givenStoreDir >>= attempting
  where
    attempting storeDir = do
      attempted <- runExceptT (attempt storeDir)
      case attempted of
        Left failure -> pure (Left failure)
        Right (Finished updated) -> pure (Right updated)
        Right Restart -> do
          TIO.hPutStrLn stderr ("profile \"" <> hostPathText profile <> "\" changed while we were busy; restarting")
          attempting storeDir
    attempt storeDir = do
      token <- ExceptT (profileToken profile)
      ExceptT (checkProfileStore storeDir profile)
      manifest <- ExceptT (readManifest profile)
      changed <- liftIO (change manifest)
      case changed of
        Unchanged -> pure (Finished NothingChanged)
        Changed next -> do
          path <- makeGeneration ops storeDir profile next
          ExceptT . withProfileLock profile . runExceptT $ do
            now <- ExceptT (profileToken profile)
            if now /= token
              then pure Restart
              else do
                generation <- createGeneration ops storeDir profile path
                ExceptT (first ProfileIOFailed <$> replaceLink profile (OP.takeFileName (generationPath generation)))
                pure (Finished (SwitchedTo generation))

-- | What a profile's link names, its raw target, or 'Nothing' when there
-- is no link: upstream's @optimisticLockProfile@ in profiles.cc.
profileToken :: OsPath -> IO (Either ProfileFailure (Maybe OsPath))
profileToken profile = do
  absent <- try (linkAbsent profile)
  case absent of
    Left (failure :: IOException) -> pure (Left (readingLink failure))
    Right True -> pure (Right Nothing)
    Right False -> do
      target <- tryIO (OsDir.getSymbolicLinkTarget profile)
      pure (either (Left . readingLink) (Right . Just) target)
  where
    readingLink failure = ProfileIOFailed ("reading symbolic link \"" <> hostPathText profile <> "\": " <> T.pack (show failure))

-- | Refuse a profile whose current generation, the path its links lead
-- to, exists outside the store.  The store directory is resolved through
-- its links as the profile is, so a store reached through a linked
-- directory, as macOS's @\/tmp@ and @\/var@ are, is its own.  A profile
-- that is not there, or whose links lead nowhere, has no generation to
-- refuse.
checkProfileStore :: StoreDir -> OsPath -> IO (Either ProfileFailure ())
checkProfileStore storeDir profile = do
  resolved <- tryIO (OsDir.canonicalizePath profile)
  case resolved of
    Left _ -> pure (Right ())
    Right path -> do
      present <- OsDir.doesPathExist path
      spelled <- OP.decodeFS path
      store <- resolvedStoreDir storeDir
      pure $
        if not present || isJust (parseStorePathPrefix store (T.pack spelled))
          then Right ()
          else Left (ForeignGeneration profile path)

-- | A store directory with every link on it resolved, as
-- @canonicalizePath@ resolves one, which is how the tree resolves a link
-- it expands; the directory as given when it cannot be resolved.
resolvedStoreDir :: StoreDir -> IO StoreDir
resolvedStoreDir storeDir = StoreDir . fromRight (unStoreDir storeDir) <$> tryIO (OP.encodeFS (unStoreDir storeDir) >>= OsDir.canonicalizePath >>= OP.decodeFS)

-- | Make a generation's store path from its manifest: the tree of its
-- active elements' paths, with the warnings the tree gives printed, and
-- @manifest.json@, added as 'generationStoreName' with every element's
-- paths as its references (@ProfileManifest::build@ in profile.cc).  The
-- tree's warnings name entries under the profile's path.  A conflict is
-- attributed under the store directory as given and as the tree resolves it.
makeGeneration :: StoreOps IO -> StoreDir -> OsPath -> Manifest -> ExceptT ProfileFailure IO StorePath
makeGeneration ops storeDir profile manifest = do
  packages <- liftIO (traverse package (manifestPackages manifest))
  Built warnings built <- liftIO (buildProfile (storeTreeReader storeDir) profile packages)
  liftIO (mapM_ (TIO.hPutStrLn stderr . ("warning: " <>) . warningText) warnings)
  tree <- case built of
    Right tree -> pure tree
    Left (FileConflict existing incoming priority) -> do
      resolvedDir <- liftIO (resolvedStoreDir storeDir)
      throwError (maybe (TreeFailed (FileConflict existing incoming priority)) TreeConflict (attributeConflict storeDir resolvedDir manifest existing incoming priority))
    Left failure -> throwError (TreeFailed failure)
  nar <- either (throwError . ProfileIOFailed) pure (generationNar hostNarTargets tree (renderManifest manifest))
  ExceptT (first StoreRefused <$> soAddSourceTree ops generationStoreName (manifestReferences manifest) nar)
  where
    package (path, priority) = (`Package` priority) <$> OP.encodeFS (storePathToFilePath storeDir path)

-- | The generation a profile switches to for a store path: the newest,
-- when it already names the path, or the next, linked and rooted as an
-- out-link is (@createGeneration@ in profiles.cc).
createGeneration :: StoreOps IO -> StoreDir -> OsPath -> StorePath -> ExceptT ProfileFailure IO Generation
createGeneration ops storeDir profile path = do
  generations <- ExceptT (listGenerations profile)
  target <- liftIO (OP.encodeFS (storePathToFilePath storeDir path))
  newest <- traverse withTarget (listToMaybe (reverse generations))
  case nextGeneration newest target of
    ReuseGeneration generation -> pure generation
    NewGeneration number -> do
      let link = generationName profile number
      linkText <- liftIO (OP.decodeFS link)
      rooted <- liftIO (soAddOutLinkRoot ops linkText path)
      either (throwError . StoreRefused) (const (pure (Generation number link))) rooted
  where
    withTarget :: Generation -> ExceptT ProfileFailure IO (Generation, OsPath)
    withTarget generation = do
      linked <- liftIO (tryIO (OsDir.getSymbolicLinkTarget (generationPath generation)))
      case linked of
        Left failure -> throwError (ProfileIOFailed ("reading symbolic link \"" <> hostPathText (generationPath generation) <> "\": " <> T.pack (show failure)))
        Right target -> pure (generation, target)

-- | Run an action holding a profile's lock, @\<profile\>.lock@
-- (@PathLocks::lockPaths@ in unix\/pathlocks.cc), with upstream's line
-- when another holds it (@lockProfile@ in profiles.cc).  A lock file that
-- cannot be opened, as in a directory that is not there, is the failure.
withProfileLock :: OsPath -> IO (Either ProfileFailure a) -> IO (Either ProfileFailure a)
withProfileLock profile action = do
  lockPath <- OP.decodeFS (profile <> ascii lockFileSuffix)
  bracket (tryIO (acquireLockFileWith LockExclusive waiting lockPath)) (traverse_ releasePathLock) (either (pure . Left . opening lockPath) (const action))
  where
    waiting = "waiting for lock on profile \"" <> T.unpack (hostPathText profile) <> "\""
    opening lockPath failure = ProfileIOFailed ("opening lock file \"" <> T.pack lockPath <> "\": " <> T.pack (show failure))

-- ---------------------------------------------------------------------------
-- Links
-- ---------------------------------------------------------------------------

-- | Whether nothing is at a path, by @lstat@: a link that leads nowhere is
-- there.
linkAbsent :: OsPath -> IO Bool
linkAbsent path = do
  probed <- try (OsDir.pathIsSymbolicLink path)
  case probed of
    Right _ -> pure False
    Left (failure :: IOException)
      | isDoesNotExistError failure -> pure True
      | otherwise -> ioError failure

-- | Point a link at a target, replacing what is there.  Every link here
-- leads to a directory, so on Windows each is a directory link whether
-- or not its target exists yet.  On Unix the link is made under a
-- temporary name beside it and renamed over the path, upstream's
-- @replaceSymlink@ in file-system.cc, which, as @rename@ does,
-- replaces a link or a file there and fails on a directory.  On Windows a
-- rename cannot replace a directory link, so a link there is removed and
-- the new one made, as an out-link is replaced ("Nix.Store.GC"), and
-- anything else there is refused.  The two steps run with asynchronous
-- exceptions masked, and a link that cannot be made puts the old one
-- back, so a profile is absent only while the switch runs.  The callers
-- replace only a link or nothing: the default profile's link is made
-- only where nothing is, and a profile's link is switched only after its
-- target was read ('profileToken'), which fails on anything but a link.
replaceLink :: OsPath -> OsPath -> IO (Either Text ())
replaceLink link target
  | onWindows = do
      absent <- linkAbsent link
      isLink <- if absent then pure False else OsDir.pathIsSymbolicLink link
      if not absent && not isLink
        then pure (Left ("cannot replace \"" <> hostPathText link <> "\", which is not a symbolic link"))
        else mask_ $ do
          previous <- if isLink then Just <$> OsDir.getSymbolicLinkTarget link else pure Nothing
          when isLink (OsDir.removePathForcibly link)
          made <- tryIO (OsDir.createDirectoryLink target link)
          case made of
            Right () -> pure (Right ())
            Left failure -> do
              traverse_ (\old -> tryIO (OsDir.createDirectoryLink old link)) previous
              pure (Left (creatingLink link failure))
  | otherwise = viaRename (0 :: Int)
  where
    viaRename n = do
      let staging = OP.takeDirectory link </> (ascii ("." <> show n <> "_") <> OP.takeFileName link)
      made <- try (OsDir.createDirectoryLink target staging)
      case made of
        Left failure
          | isAlreadyExistsError failure -> viaRename (n + 1)
          | otherwise -> pure (Left (creatingLink staging failure))
        Right () -> do
          renamed <- try (OsDir.renamePath staging link `onException` OsDir.removeFile staging)
          pure (first (\failure -> "renaming \"" <> hostPathText staging <> "\" to \"" <> hostPathText link <> "\": " <> T.pack (show (failure :: IOException))) renamed)
    creatingLink path failure = "creating symlink \"" <> hostPathText path <> "\" -> \"" <> hostPathText target <> "\": " <> T.pack (show (failure :: IOException)) <> windowsHint
    windowsHint
      | onWindows = " (on Windows this needs Developer Mode or elevation)"
      | otherwise = ""

-- | Whether this is Windows, where a link's flavour and its replacement
-- differ.  A plain comparison rather than CPP, so both branches are
-- checked on every host.
onWindows :: Bool
onWindows = System.Info.os == "mingw32"

-- | 'try' for the one exception file system calls throw.
tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | An 'OsPath' spelled from ASCII, which 'OP.unsafeFromChar' narrows to
-- one path unit per character without loss.
ascii :: String -> OsPath
ascii = OP.pack . map OP.unsafeFromChar
