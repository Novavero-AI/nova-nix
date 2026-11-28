{-# LANGUAGE CPP #-}

-- | The Windows job an unsandboxed builder runs in, which the process
-- library makes for each builder ('System.Process.use_process_jobs') and
-- every process the builder starts joins.
--
-- A process that meets an unhandled exception, as a configure test that
-- divides by zero to see whether that raises SIGFPE does, has Windows
-- Error Reporting start WerFault.exe from inside it: a program from
-- outside the store, which can wait on a dialog no one answers.  Upstream
-- has a crashing builder dump no core.  The job's
-- @JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION@ is the counterpart here:
-- Windows' unhandled-exception filter then returns to the process's own
-- handler, which ends it with the exception's code as its exit code,
-- without invoking the reporter.  An error mode would not hold: the one a
-- process passes to those it starts (@SEM_NOGPFAULTERRORBOX@) is one each
-- may replace, as busybox-w32 replaces it with @SEM_FAILCRITICALERRORS@
-- alone when it starts, while the limit is the job's.  The sandbox helper
-- gives its own job the same limit (sandbox\/windows.c).
--
-- The process library starts the builder before it hands back the job,
-- so the limit is set a moment after the builder starts.
module Nix.Builder.Job
  ( dieOnUnhandledException,
  )
where

import qualified System.Process as Proc

#if defined(mingw32_HOST_OS)

import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.Process.Internals (ProcessHandle__ (..), withProcessHandle)
import System.Win32.Types (ErrCode, failWith)

#endif

-- | Have the job a builder was started in end each of its processes at
-- an unhandled exception, without Windows Error Reporting, keeping the
-- job's other limits.  A failure throws the 'IOError' naming the call
-- that failed, as a builder that could not be started does.  A process
-- in no job of the process library's, as one started without
-- 'Proc.use_process_jobs' is and every process off Windows is, is left
-- as it is.
dieOnUnhandledException :: Proc.ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
dieOnUnhandledException process = withProcessHandle process $ \handle -> case handle of
  OpenExtHandle _ job -> alloca $ \failedOut -> do
    code <- c_nn_job_die_on_unhandled_exception job failedOut
    case code of
      0 -> pure ()
      _ -> peek failedOut >>= peekCString >>= \call -> failWith call code
  OpenHandle _ -> pure ()
  ClosedHandle _ -> pure ()

foreign import ccall unsafe "nn_winfs.h nn_job_die_on_unhandled_exception"
  c_nn_job_die_on_unhandled_exception :: Ptr () -> Ptr CString -> IO ErrCode
#else
dieOnUnhandledException _ = pure ()
#endif
