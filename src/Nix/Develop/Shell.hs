{-# LANGUAGE CPP #-}

-- | The shell @nova-nix develop@ starts, as upstream Nix 2.35.4's
-- @CmdDevelop::run@ starts it (@src\/nix\/develop.cc@) through
-- @execProgramInStore@ (@src\/nix\/run.cc@).
--
-- What the shell runs, its arguments and the environment it starts with
-- are pure: 'shellModeFor', 'shellArguments' and 'shellEnvironment', each
-- refusing what upstream refuses in upstream's words.  The rest belongs
-- to the host, a t'DevelopHost': the temporary files upstream names, the
-- process's environment, and the shell itself, which replaces nova-nix
-- as upstream's replaces @nix@, so that the shell has the terminal, its
-- signals and its exit status to itself.  Only a POSIX host has one.  No
-- Windows stdenv's builder is bash yet, so on Windows neither @develop@
-- nor @print-dev-env@ has a capture to read or a shell to start.
module Nix.Develop.Shell
  ( -- * What the shell runs
    shellModeFor,
    bashProgram,
    shellArguments,

    -- * The shell's environment
    EnvironmentBase (..),
    EnvironmentChanges (..),
    shellEnvironment,

    -- * The host
    DevelopHost (..),
    developHost,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
#if defined(mingw32_HOST_OS)
import Nix.Develop.Environment (Prompt, ShellMode (..))
#else
import Control.Exception (bracket, catchJust, try, tryJust)
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word32)
import Foreign.C.Error (Errno (..), eBADF, eINVAL)
import Foreign.C.Types (CInt)
import GHC.IO.Exception (IOException (..))
import Nix.Develop.Environment (Prompt, ShellMode (..), appendPath)
import qualified System.Directory.OsPath as OsDir
import System.IO (hClose, hFlush, stderr, stdout)
import System.IO.Error (isAlreadyExistsError)
import qualified System.OsPath as OP
import System.Posix.Directory.ByteString (closeDirStream, createDirectory, openDirStream, readDirStream)
import qualified System.Posix.Env.ByteString as PosixEnv
import System.Posix.IO (FdOption (CloseOnExec), setFdOption)
import System.Posix.Process.ByteString (executeFile, getProcessID)
import System.Posix.Resource (Resource (ResourceOpenFiles), ResourceLimit (..), getResourceLimit, softLimit)
import System.Posix.Temp.ByteString (mkstemp)
import System.Posix.Types (Fd (..), FileMode)
import System.Posix.Unistd (SysVar (OpenFileLimit), getSysVar)
import System.Random (randomIO)
import Text.Read (readMaybe)
#endif

-- ---------------------------------------------------------------------------
-- What the shell runs
-- ---------------------------------------------------------------------------

-- | What the shell does once the environment is set, from the words
-- @--command@ took and the phase the phase flags named last, as
-- upstream's @CmdDevelop::run@ decides it (develop.cc): the phase, the
-- command, or else an interactive shell with the prompt settings.  Upstream
-- refuses the two together once the environment is built, in these words.
shellModeFor :: Maybe (NonEmpty ByteString) -> Maybe ByteString -> Prompt -> Either Text ShellMode
shellModeFor command phase prompt = case (command, phase) of
  (Just _, Just _) -> Left "you cannot use both '--command' and '--phase'"
  (Nothing, Just name) -> Right (RunPhase name)
  (Just commandWords, Nothing) -> Right (RunCommand commandWords)
  (Nothing, Nothing) -> Right (Interactive prompt)

-- | The shell upstream starts when @nixpkgs#bashInteractive@ does not
-- resolve, as it cannot without flakes (@CmdDevelop::run@ in develop.cc):
-- @bash@, found on the @PATH@ of the environment the shell starts with.  It
-- is also what @SHELL@ holds.
bashProgram :: ByteString
bashProgram = "bash"

-- | The shell's arguments after its name (@CmdDevelop::run@ in develop.cc):
-- the rcfile alone for a command or a phase, which bash then runs as a
-- script, so that an interrupt ends it rather than leaving an interactive
-- shell behind; @--rcfile@ and the rcfile for an interactive shell.
shellArguments :: ShellMode -> ByteString -> [ByteString]
shellArguments mode rcfile = case mode of
  Interactive _ -> ["--rcfile", rcfile]
  RunCommand _ -> [rcfile]
  RunPhase _ -> [rcfile]

-- ---------------------------------------------------------------------------
-- The shell's environment
-- ---------------------------------------------------------------------------

-- | What the shell's environment starts from.
data EnvironmentBase
  = -- | The caller's: the environment nova-nix was started with.
    CallerEnvironment
  | -- | Only the variables @--keep-env-var@ names: @--ignore-env@.
    EmptyEnvironment
  deriving (Eq, Show)

-- | How the command line changes the shell's environment, upstream's
-- @MixEnvironment@ (its constructor in @src\/libcmd\/command.cc@, from
-- 2.34.9).  A variable is named by its bytes.
data EnvironmentChanges = EnvironmentChanges
  { -- | Where it starts: @--ignore-env@.
    ecBase :: !EnvironmentBase,
    -- | @--keep-env-var@: the caller's variables kept under
    -- @--ignore-env@.
    ecKeep :: !(Set ByteString),
    -- | @--unset-env-var@: the variables removed.
    ecUnset :: !(Set ByteString),
    -- | @--set-env-var@: each variable set, with its value.
    ecSet :: !(Map ByteString ByteString)
  }
  deriving (Eq, Show)

-- | The environment the shell starts with, given the store path of the
-- environment the capture built and the caller's environment: upstream's
-- @setEnviron@ (command.cc), the variables @--set-env-var@ sets replacing the
-- caller's and those @--unset-env-var@ names removed after, then @NIX_GCROOT@
-- naming the store path and @SHELL@ the shell, whatever the flags said of
-- either (@CmdDevelop::run@ in develop.cc).  Upstream replaces the process's
-- environment through @setenv@, which refuses a name that is empty or holds
-- @=@, and goes on without it; such a name is left out here.
--
-- 'Left' in upstream's words for @--unset-env-var@ under
-- @--ignore-env@, and for @--keep-env-var@ without it.
shellEnvironment :: EnvironmentChanges -> ByteString -> Map ByteString ByteString -> Either Text (Map ByteString ByteString)
shellEnvironment changes gcroot caller = case ecBase changes of
  EmptyEnvironment
    | not (Set.null (ecUnset changes)) -> Left "--unset-env-var does not make sense with --ignore-env"
    | otherwise -> Right (finish (Map.restrictKeys caller (ecKeep changes)))
  CallerEnvironment
    | not (Set.null (ecKeep changes)) -> Left "--keep-env-var does not make sense without --ignore-env"
    | otherwise -> Right (finish caller)
  where
    finish base =
      Map.filterWithKey (const . settable)
        $ Map.insert shellVariable bashProgram . Map.insert gcrootVariable gcroot
        $ Map.withoutKeys (Map.union (ecSet changes) base) (ecUnset changes)
    settable name = not (BC.null name) && BC.notElem '=' name

-- | The variable naming the environment's store path, which upstream's
-- collector on Linux keeps while a process's environment holds it
-- (@findRuntimeRootsUnchecked@ in @src\/libstore\/local-gc.cc@ from 2.34.9,
-- @LocalStore::findRuntimeRoots@ in gc.cc at 2.24.9).
gcrootVariable :: ByteString
gcrootVariable = "NIX_GCROOT"

-- | The variable naming the shell.
shellVariable :: ByteString
shellVariable = "SHELL"

-- ---------------------------------------------------------------------------
-- The host
-- ---------------------------------------------------------------------------

-- | What @develop@ and @print-dev-env@ ask of the host beyond its store:
-- the temporary files upstream makes, the process's environment, and a
-- shell that replaces the process.  'developHost' gives this host's,
-- where it has one.
data DevelopHost = DevelopHost
  { -- | Make a fresh directory @PREFIX-PID-N@, mode 0755 less the umask,
    -- in the temporary root with its links resolved, as upstream's
    -- @createTempDir@ makes one (with @makeTempPath@, in
    -- @src\/libutil\/file-system.cc@ from 2.34.9), @N@ counting up from a
    -- random number past each name that exists, and give its path's bytes.
    makeTemporaryDirectory :: !(ByteString -> IO ByteString),
    -- | Make a fresh file @nix-shell.XXXXXX@ in the temporary root, mode
    -- 0600, as upstream's @createTempFile@ makes the rcfile
    -- (file-system.cc), write to it what the function makes of its path,
    -- close it and give the path's bytes.
    createRcfile :: !((ByteString -> ByteString) -> IO ByteString),
    -- | The process's environment, as upstream's @getEnv@ reads it: an
    -- entry without @=@ is passed over, and of two entries with one name
    -- the first is kept.
    processEnvironment :: !(IO (Map ByteString ByteString)),
    -- | Replace the process with 'bashProgram' and these arguments, in
    -- this environment.  It returns only by throwing the failure to start
    -- the shell.
    replaceProcess :: !([ByteString] -> Map ByteString ByteString -> IO Void)
  }

-- | A t'DevelopHost' for this host, or why it has none.
developHost :: Either Text DevelopHost
#if defined(mingw32_HOST_OS)
developHost = Left "develop and print-dev-env do not run on Windows yet"
#else
developHost = Right (DevelopHost posixTemporaryDirectory posixRcfile posixEnvironment posixReplace)

-- | Upstream's @defaultTempDir@ (@src\/libutil\/unix\/file-system.cc@ from
-- 2.34.9, file-system.cc at 2.24.9): @TMPDIR@ when it is set and not empty,
-- else @\/tmp@.
temporaryRoot :: IO ByteString
temporaryRoot = fromMaybe "/tmp" . (>>= nonEmpty) <$> PosixEnv.getEnv "TMPDIR"
  where
    nonEmpty value
      | BS.null value = Nothing
      | otherwise = Just value

-- | The mode upstream's @createTempDir@ gives a directory.
temporaryDirectoryMode :: FileMode
temporaryDirectoryMode = 0o755

-- | 'makeTemporaryDirectory'.  The root's links are resolved as upstream's
-- @canonPath@ resolves them, through the directory package's @realpath@
-- on the root's own bytes.
posixTemporaryDirectory :: ByteString -> IO ByteString
posixTemporaryDirectory prefix = do
  root <- temporaryRoot >>= fmap osPathBytes . OsDir.canonicalizePath . bytesOsPath
  pid <- getProcessID
  start <- randomIO
  let attempt :: Word32 -> IO ByteString
      attempt counter = do
        let path = appendPath root (prefix <> "-" <> BC.pack (show pid) <> "-" <> BC.pack (show counter))
        made <- tryJust (guard . isAlreadyExistsError) (createDirectory path temporaryDirectoryMode)
        either (const (attempt (counter + 1))) (const (pure path)) made
  attempt start

-- | 'createRcfile'.  The root is upstream's, its links left as they are.
posixRcfile :: (ByteString -> ByteString) -> IO ByteString
posixRcfile render = do
  root <- temporaryRoot
  bracket (mkstemp (appendPath root rcfilePrefix)) (hClose . snd) $ \(path, handle) ->
    path <$ BS.hPut handle (render path)

-- | What the rcfile's name starts with, before the six characters
-- @mkstemp@ adds.
rcfilePrefix :: ByteString
rcfilePrefix = "nix-shell."

-- | 'processEnvironment'.
posixEnvironment :: IO (Map ByteString ByteString)
posixEnvironment = Map.fromListWith (\_ earlier -> earlier) . concatMap entry <$> PosixEnv.getEnvironmentPrim
  where
    entry raw = case BC.break (== '=') raw of
      (name, rest) | Just ('=', value) <- BC.uncons rest -> [(name, value)]
      _ -> []

-- | 'replaceProcess': the standard handles flushed, every variable unset
-- and the new ones set, as upstream's @replaceEnv@ does
-- (with @clearEnv@, in @src\/libutil\/environment-variables.cc@), every other
-- descriptor marked close-on-exec ('closeOnExec'), then @execvp@, which finds
-- the shell on the new environment's @PATH@, as upstream's does
-- (@execProgramInStore@ in run.cc).  unix's @executeFile@ with no environment
-- is that call; given one it is @execvpe@, which searches the old @PATH@.  An
-- entry @unsetenv@ refuses, one with an empty name, stays, as upstream goes
-- on past the refusal.
--
-- GHC's runtime installs its own handlers for SIGINT, SIGPIPE, SIGQUIT
-- and SIGTSTP at start, whatever the process inherited, an ignored
-- signal included (@initDefaultHandlers@ and @set_sigtstp_action@ in
-- rts\/posix\/Signals.c at 9.14.1), and the exec resets a handled
-- signal to its default.  So the shell starts with those four at their
-- defaults even where nova-nix was started with one ignored.  Upstream
-- leaves them as it found them on Linux, so an ignored one stays ignored
-- in its shell, and sets SIGINT, SIGPIPE and SIGQUIT to their defaults on
-- macOS (@initNix@ in @src\/libmain\/shared.cc@ at 2.35.4).
posixReplace :: [ByteString] -> Map ByteString ByteString -> IO Void
posixReplace arguments variables = do
  hFlush stdout
  hFlush stderr
  posixEnvironment >>= mapM_ (passingOver eINVAL . PosixEnv.unsetEnv) . Map.keys
  mapM_ (\(name, value) -> PosixEnv.setEnv name value True) (Map.toList variables)
  closeOnExec
  executeFile bashProgram True arguments Nothing

-- | Mark every descriptor from 3 up close-on-exec, so that none the
-- process holds reaches the shell: the HTTP client's pool keeps a
-- connection a substitution used open, without the flag.  The descriptors
-- are those the process's descriptor directory lists, @\/proc\/self\/fd@
-- on Linux and @\/dev\/fd@ on macOS, or where neither can be read, every
-- number below the soft limit on open files, or below @OPEN_MAX@ when
-- that limit is infinite, and never past the largest a descriptor can
-- be.  One closed since it was listed, as the listing's own is, is passed
-- over.
closeOnExec :: IO ()
closeOnExec = do
  listed <- firstListing descriptorDirectories
  descriptors <- maybe everyDescriptor pure listed
  mapM_ (\descriptor -> passingOver eBADF (setFdOption (Fd descriptor) CloseOnExec True)) (filter (>= firstOtherDescriptor) descriptors)
  where
    firstListing [] = pure Nothing
    firstListing (directory : rest) = descriptorListing directory >>= maybe (firstListing rest) (pure . Just)
    everyDescriptor = do
      limits <- getResourceLimit ResourceOpenFiles
      bound <- case softLimit limits of
        ResourceLimit limit -> pure limit
        _ -> getSysVar OpenFileLimit
      pure [firstOtherDescriptor .. fromInteger (min bound (toInteger (maxBound :: CInt))) - 1]

-- | The first descriptor past standard input, output and error.
firstOtherDescriptor :: CInt
firstOtherDescriptor = 3

-- | The directories that list a process's own descriptors, by name.
descriptorDirectories :: [ByteString]
descriptorDirectories = ["/proc/self/fd", "/dev/fd"]

-- | The descriptors a directory lists, or 'Nothing' when it cannot be
-- read.
descriptorListing :: ByteString -> IO (Maybe [CInt])
descriptorListing directory = do
  listed <- try (bracket (openDirStream directory) closeDirStream readAll) :: IO (Either IOException [ByteString])
  pure (either (const Nothing) (Just . mapMaybe (readMaybe . BC.unpack)) listed)
  where
    readAll stream = do
      entry <- readDirStream stream
      if BS.null entry then pure [] else (entry :) <$> readAll stream

-- | Run an action, passing over its failure with this errno and no
-- other.
passingOver :: Errno -> IO () -> IO ()
passingOver expected action = catchJust (\failure -> guard (fmap Errno (ioe_errno failure) == Just expected)) action pure

-- | The host path a POSIX path's bytes name: each byte one unit.
bytesOsPath :: ByteString -> OP.OsPath
bytesOsPath = OP.pack . map OP.unsafeFromChar . BC.unpack

-- | A POSIX host path's bytes, the inverse of 'bytesOsPath'.
osPathBytes :: OP.OsPath -> ByteString
osPathBytes = BC.pack . map OP.toChar . OP.unpack
#endif
