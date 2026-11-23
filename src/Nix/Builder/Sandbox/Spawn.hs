{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The build sandbox's IO: whether this system can sandbox a build, the
-- chroot a sandboxed build runs in, and the one warning a process prints
-- when its builds fall back to running unsandboxed.  What is decided is
-- "Nix.Builder.Sandbox"; this module only carries it out.
--
-- == Where it runs
--
-- Only Linux has a sandbox nova-nix makes ('sandboxSupported').  The
-- chroot code is POSIX and builds everywhere but Windows, so it runs in
-- the test suite on macOS as well; on Windows it refuses.
--
-- == Process state
--
-- Upstream probes the namespaces once per process and keeps the answer
-- (namespaces.cc:18 and 63 at 2.24.9, function statics).  The answer here
-- is kept in one process-wide cell ('probeCache'), with whether the
-- fallback warning was printed: both describe this process and this
-- system, not a build, and asking again for every derivation of a closure
-- would run the probe and print the warning once per derivation.
module Nix.Builder.Sandbox.Spawn
  ( -- * Availability
    sandboxSupported,
    sandboxAvailability,
    warnFallback,
    processIsRoot,

    -- * Running the helper
    HelperRun (..),
    runHelper,

    -- * The chroot
    chrootDirFor,
    withChroot,
    mountPointsWithin,
    sealChroot,

    -- * Paths as bytes
    filePathBytes,
    bytesFilePath,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (IOException, bracket, displayException, try)
import qualified Control.Exception as E
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Nix.Builder.Log (openLogPipe, streamBuildLog)
import Nix.Builder.Sandbox (SkeletonEntry (..), Unavailability (..), fallbackWarning, parseStatus, probeStageName)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hFlush, stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Proc
#if !defined(mingw32_HOST_OS)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Directory (createDirectoryIfMissing, listDirectory, removePathForcibly)
import System.FilePath (splitDirectories, (</>))
import System.Posix.Directory (createDirectory)
import System.Posix.Files (createSymbolicLink, fileOwner, getSymbolicLinkStatus, isDirectory, setFileMode)
import Nix.Builder.BuildDir (clearLeftover)
import Nix.Store.Metadata (writableByOthers)
import System.Posix.Types (FileMode)
import System.Posix.User (getEffectiveUserID, getRealUserID)
#endif

-- ---------------------------------------------------------------------------
-- Availability
-- ---------------------------------------------------------------------------

-- | Whether this platform has a sandbox nova-nix makes.
sandboxSupported :: Bool
#if defined(linux_HOST_OS)
sandboxSupported = True
#else
sandboxSupported = False
#endif

-- | What the probe found for a helper, and whether the fallback warning
-- has been printed.
data ProbeState = ProbeState
  { psAnswers :: !(Map.Map FilePath (Either Unavailability FilePath)),
    psWarned :: !Bool
  }

-- | The process's probe answers and warning.  A NOINLINE top-level cell
-- made once, by 'unsafePerformIO': it holds only what this process
-- learned about the system it runs on, and every reader takes it through
-- 'modifyMVar', so the answer is the same however many threads build.
probeCache :: MVar ProbeState
probeCache = unsafePerformIO (newMVar (ProbeState Map.empty False))
{-# NOINLINE probeCache #-}

-- | Whether a sandboxed build can run here, and which helper runs it: on
-- a platform with a sandbox, with the helper found, its probe passing.
-- The probe runs once per process for each helper, the first time a
-- sandboxed build asks.
sandboxAvailability :: Maybe FilePath -> IO (Either Unavailability FilePath)
sandboxAvailability helper
  | not sandboxSupported = pure (Left PlatformUnsupported)
  | otherwise = case helper of
      Nothing -> pure (Left HelperNotFound)
      Just path -> modifyMVar probeCache $ \state -> case Map.lookup path (psAnswers state) of
        Just answer -> pure (state, answer)
        Nothing -> do
          answer <- probe path
          pure (state {psAnswers = Map.insert path answer (psAnswers state)}, answer)

-- | Run the helper's probe.
probe :: FilePath -> IO (Either Unavailability FilePath)
probe helper = do
  ran <- try (Proc.readCreateProcessWithExitCode ((Proc.proc helper ["probe"]) {Proc.env = Just []}) "")
  pure $ case ran of
    Left (failure :: IOException) -> Left (HelperUnrunnable (T.pack (show failure)))
    Right (ExitSuccess, "", _) -> Right helper
    Right (_, out, _) -> case parseStatus probeStageName (BS8.pack out) of
      Right (Just record) -> Left (ProbeFailed record)
      Right Nothing -> Left (ProbeUnreadable "the sandbox helper's probe failed and said nothing")
      Left reason -> Left (ProbeUnreadable reason)

-- | Print the fallback warning ('fallbackWarning'), the first time this
-- process falls back.
warnFallback :: Unavailability -> IO ()
warnFallback cause = do
  first <- modifyMVar probeCache $ \state -> pure (state {psWarned = True}, not (psWarned state))
  when first (TIO.hPutStrLn stderr (fallbackWarning cause))

-- | Upstream's @isRootUser@ (users.cc at 2.24.9): whether the process
-- runs as uid 0, which is where @require-drop-supplementary-groups@
-- defaults on.  Never on Windows, which has no uid.
processIsRoot :: IO Bool
#if defined(mingw32_HOST_OS)
processIsRoot = pure False
#else
processIsRoot = (== 0) <$> getRealUserID
#endif

-- ---------------------------------------------------------------------------
-- Running the helper
-- ---------------------------------------------------------------------------

-- | What a run of the helper came to: whether the plan reached it, how it
-- exited, and what it wrote on its status output.
data HelperRun = HelperRun
  { hrDelivered :: !(Either Text ()),
    hrExit :: !ExitCode,
    hrStatus :: !ByteString
  }
  deriving (Eq, Show)

-- | Run the helper over a builder, given the build log, the helper, the
-- plan, the directory it starts in, and the program and arguments it
-- executes.  It is spawned as a builder is ("Nix.Builder"), with its
-- protocol around it ("Nix.Builder.Sandbox"): an empty environment, so the
-- loader reads no variable a derivation set; the plan on its standard
-- input, which stays open after it as the lifeline the helper kills the
-- build on the end of; its standard output the status pipe; and standard
-- error the log, which the helper makes the builder's standard output too.
-- The status is read once the log ends and the helper is gone, since it
-- holds at most one short record, which the pipe buffers.
--
-- Whatever ends the run early, an interrupt included, the helper is sent
-- @SIGTERM@ and waited for before this returns, with asynchronous
-- exceptions masked so a second one cannot cut the wait short.  The helper
-- answers by killing the builder, and exits only once it has reaped it:
-- the builder is PID 1 of its namespace, which the kernel empties before
-- PID 1 can be reaped, so when this returns no process of the build is
-- left to write into its directory or chroot.
runHelper :: Handle -> FilePath -> ByteString -> FilePath -> [String] -> IO HelperRun
runHelper buildLog helper plan workDir command =
  bracket openLogPipe closePipe $ \(fromBuilder, toLog) ->
    E.mask $ \restore -> do
      (stdinEnd, stdoutEnd, _, process) <- Proc.createProcess (spawnWith toLog)
      let release = for_ stdinEnd hCloseQuietly *> for_ stdoutEnd hCloseQuietly
          stop = E.uninterruptibleMask_ (Proc.terminateProcess process *> void (Proc.waitForProcess process))
      outcome <-
        restore
          ( do
              delivered <- maybe (pure (Left "the sandbox helper has no standard input")) deliver stdinEnd
              streamBuildLog fromBuilder buildLog
              exitCode <- Proc.waitForProcess process
              status <- maybe (pure BS.empty) BS.hGetContents stdoutEnd
              pure (HelperRun delivered exitCode status)
          )
          `E.onException` (stop *> release)
      release
      pure outcome
  where
    spawnWith toLog =
      (Proc.proc helper (helperRunVerb : command))
        { Proc.cwd = Just workDir,
          Proc.env = Just [],
          Proc.std_in = Proc.CreatePipe,
          Proc.std_out = Proc.CreatePipe,
          Proc.std_err = Proc.UseHandle toLog
        }
    closePipe (fromBuilder, toLog) = hClose fromBuilder *> hClose toLog
    -- A helper that ends before it reads the plan closes the pipe under
    -- the write; what it reported, if anything, says why.  The handle
    -- stays open: it is the lifeline.
    deliver planIn = do
      written <- try (BS.hPut planIn plan *> hFlush planIn)
      pure $ case written of
        Left (failure :: IOException) -> Left (T.pack (displayException failure))
        Right () -> Right ()
    -- Closing a pipe whose reader is gone can fail on the flush; the run
    -- is over either way.
    hCloseQuietly handle = hClose handle `E.catch` \(_ :: IOException) -> pure ()

-- | The helper's verb for running a builder.
helperRunVerb :: String
helperRunVerb = "run"

-- ---------------------------------------------------------------------------
-- The chroot
-- ---------------------------------------------------------------------------

-- | Where the chroot of a build whose directory is given lives: beside it,
-- in the store's state directory, so the outputs written inside move into
-- the store by a rename, as upstream's chroot sits in the store
-- (LDG 692).
chrootDirFor :: FilePath -> FilePath
chrootDirFor buildDir = buildDir <> ".chroot"

-- | The mount points at or beneath a directory, read from
-- @\/proc\/self\/mountinfo@'s text: the fifth field of each line, its
-- fields split at single spaces, as the kernel writes them, with the
-- kernel's octal escapes for a space, a tab, a newline and a backslash
-- undone.  The helper's walk of a bind's mounts reads the text the same
-- way (@sandbox\/mountinfo.c@).  A chroot's binds are mounts of the build's own namespace, gone
-- once the build is, so a mount point here means something was mounted
-- into the chroot from outside, and removing the chroot through it could
-- reach the store or the host.
mountPointsWithin :: ByteString -> ByteString -> [ByteString]
mountPointsWithin mountinfo dir =
  [point | line <- BS8.lines mountinfo, _ : _ : _ : _ : field : _ <- [BS8.split ' ' line], let point = unescape field, within point]
  where
    within point = point == dir || (dir <> "/") `BS.isPrefixOf` point
    unescape field = case BS8.break (== '\\') field of
      (plain, rest) -> case BS8.uncons rest of
        Nothing -> plain
        Just (_, afterSlash)
          | Just byte <- octal (BS.take 3 afterSlash) -> plain <> BS.singleton byte <> unescape (BS.drop 3 afterSlash)
          | otherwise -> plain <> "\\" <> unescape afterSlash
    octal digits
      | BS.length digits == 3, BS8.all (`elem` ['0' .. '7']) digits = Just (fromIntegral (BS.foldl' (\acc d -> acc * 8 + fromIntegral d - 48) (0 :: Int) digits))
      | otherwise = Nothing

-- | Close a sandboxed build's chroot to every other user, once its builder
-- has ended and no process is left in its PID namespace, and before
-- anything reads an output there.  A fixed-output build keeps the host's
-- network, so its builder can have opened a directory of the chroot to
-- everyone and passed another user a descriptor for it over an abstract
-- socket; with one, that user could rename entries while nova-nix walks,
-- copies and moves an output by path, and lead the walk out of the chroot.
--
-- From the root down to the store directory inside the chroot, each
-- directory has to be one of the invoking user's own, not a link, and is
-- made 0700.  Then every directory of the user's own in the store
-- directory's subtree, from the top down, is made 0700 too: no other
-- user can then write it, or reach through it a file the builder left
-- writable by everyone, and its owner can enter it, as the walk and what
-- reads the outputs after it need, if the builder closed it.  Nothing
-- needs another user to reach it before an output is made canonical and
-- moved into the store.  A directory is changed by its path, read without following a
-- link, and only once its parent is sealed, so no other user can point
-- that path elsewhere in between; the root's parent is no build's to
-- reach.  A directory another user owns is not entered, and an output
-- holding one is refused ('Nix.Builder').  Nothing outside the store
-- directory's subtree is read, since registration reads nothing else and
-- the chroot's removal is descriptor-relative.
--
-- Answers the names of the store directory's entries the builder left
-- writable by others, read before the seal changes their modes, for the
-- refusal of an output others could have changed.
--
-- That closes the chroot to other users.  It does not close it to a
-- process of the same user, a concurrent build among them, which may
-- change anything there as the user may.
sealChroot :: FilePath -> FilePath -> IO (Either Text (Set FilePath))
#if defined(mingw32_HOST_OS)
sealChroot _ _ = pure (Right Set.empty)
#else
sealChroot root storeDir = do
  user <- getEffectiveUserID
  let components = filter (`notElem` ["/", ""]) (splitDirectories storeDir)
      chain = scanl (</>) root components
      storeInChroot = foldl' (</>) root components
      ownDirectory status = isDirectory status && fileOwner status == user
      sealChain [] = pure (Right ())
      sealChain (path : rest) = do
        status <- E.try (getSymbolicLinkStatus path)
        case status of
          Right found | ownDirectory found -> setFileMode path privateMode *> sealChain rest
          Right _ -> pure (Left (replaced path))
          Left (_ :: IOException) -> pure (Left (replaced path))
      sealBelow path = do
        entries <- listDirectory path
        for_ entries $ \entry -> do
          let next = path </> entry
          status <- getSymbolicLinkStatus next
          when (ownDirectory status) $ do
            setFileMode next privateMode
            sealBelow next
      seal = do
        entries <- listDirectory storeInChroot
        marked <- traverse (\entry -> (,) entry <$> writableByOthers (storeInChroot </> entry)) entries
        sealBelow storeInChroot
        pure (Set.fromList [entry | (entry, True) <- marked])
  sealed <- E.try (sealChain chain >>= either (pure . Left) (const (Right <$> seal)))
  pure $ case sealed of
    Left (failure :: IOException) -> Left (T.pack ("cannot close the sandbox to other users: " <> displayException failure))
    Right result -> result
  where
    privateMode = 0o700
    replaced path = "the build replaced '" <> T.pack path <> "' in its sandbox, which holds its outputs, with something other than a directory of its own"
#endif

-- | Run a sandboxed build's action in its chroot, made empty and filled
-- with the skeleton: what an earlier build left at the name is removed
-- first, and the chroot is removed when the action ends, however it ends.
-- The action is given the chroot's root.  'Left' is why the chroot could
-- not be made.
--
-- Removal goes through @removePathForcibly@, which since directory 1.3.9
-- works relative to each directory's descriptor and never follows a link,
-- so a directory the build swapped for a link is unlinked, not entered.
-- It is refused while anything is mounted in the chroot
-- ('mountPointsWithin').
withChroot :: FilePath -> [SkeletonEntry] -> (FilePath -> IO a) -> IO (Either Text a)
#if defined(mingw32_HOST_OS)
withChroot _ _ _ = pure (Left "sandboxing builds is not supported on this platform")
#else
withChroot buildDir skeleton action = do
  leftover <- removeChroot clearLeftover parent
  case leftover of
    Left reason -> pure (Left reason)
    Right () -> do
      made <- try makeChroot
      case made of
        Left (failure :: IOException) -> do
          _ <- removeChroot removePathForcibly parent
          pure (Left ("cannot create the sandbox's root '" <> T.pack root <> "': " <> T.pack (show failure)))
        Right () -> Right <$> (action root `E.finally` removeQuietly)
  where
    parent = chrootDirFor buildDir
    root = parent </> "root"
    makeChroot = do
      createDirectory parent privateDir
      createDirectory root rootDir
      setFileMode root rootDir
      for_ skeleton (materialize root)
    -- The removal runs as the build ends, where a chroot that will not
    -- go must not replace the build's own outcome; what it leaves is
    -- removed, or refused, by the next build of the derivation.
    removeQuietly = do
      outcome <- try (removeChroot removePathForcibly parent)
      case outcome of
        Left (_ :: IOException) -> pure ()
        Right _ -> pure ()
    privateDir = 0o700
    rootDir = 0o750

-- | Remove a chroot, unless something is mounted in it, by a removal of
-- the caller's: a leftover at the start of a build may be moved aside
-- ('clearLeftover').
removeChroot :: (FilePath -> IO ()) -> FilePath -> IO (Either Text ())
removeChroot remove parent = do
  parentBytes <- filePathBytes parent
  mountinfo <- E.try (BS.readFile "/proc/self/mountinfo")
  case either (\(_ :: IOException) -> []) (`mountPointsWithin` parentBytes) mountinfo of
    [] -> Right <$> remove parent
    point : _ -> pure (Left ("refusing to remove '" <> T.pack parent <> "', since '" <> TE.decodeUtf8Lenient point <> "' is mounted in it"))

-- | Make one skeleton entry beneath the root.
materialize :: FilePath -> SkeletonEntry -> IO ()
materialize root entry = case entry of
  SkeletonDir path mode -> do
    at <- inRoot path
    createDirectoryIfMissing True at
    setFileMode at (fromIntegral mode :: FileMode)
  SkeletonFile path contents -> do
    at <- inRoot path
    BS.writeFile at contents
  SkeletonLink path target -> do
    at <- inRoot path
    targetPath <- bytesFilePath target
    createSymbolicLink targetPath at
  where
    inRoot path = (root <>) <$> bytesFilePath path
#endif

-- ---------------------------------------------------------------------------
-- Paths as bytes
-- ---------------------------------------------------------------------------

-- | A path's bytes: on POSIX what the file system encoding gives, which
-- returns a name read from the system to the bytes it was read from; on
-- Windows its UTF-8.
filePathBytes :: FilePath -> IO ByteString
#if defined(mingw32_HOST_OS)
filePathBytes = pure . TE.encodeUtf8 . T.pack
#else
filePathBytes path = do
  encoding <- getFileSystemEncoding
  GHC.withCStringLen encoding path BS.packCStringLen
#endif

-- | The path bytes name, the inverse of 'filePathBytes'.
bytesFilePath :: ByteString -> IO FilePath
#if defined(mingw32_HOST_OS)
bytesFilePath = pure . T.unpack . TE.decodeUtf8Lenient
#else
bytesFilePath bytes = do
  encoding <- getFileSystemEncoding
  BS.useAsCStringLen bytes (GHC.peekCStringLen encoding)
#endif
