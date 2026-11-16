-- | The roots of an evaluation session in the C data layer
-- (@cbits\/nn_roots.h@): each C object Haskell keeps beyond one
-- evaluation step, registered as it is made, so a mark over the C graph
-- starts from it without the caller naming it ("Nix.Eval.CStats").  A
-- session registers its base environment and search path as it starts,
-- and each imported file's value as the import cache takes it
-- ("Nix.Eval.IO").  The arena's teardown forgets them all.
module Nix.Eval.CRoots
  ( crootsAddThunk,
    crootsAddEnv,
    crootsClear,
  )
where

import Control.Monad (unless)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CEnv (NnEnv)
import Nix.Eval.CStatus (cStatusFailure)
import Nix.Eval.CThunk (CThunkPtr)

foreign import ccall unsafe "nn_roots_add_thunk"
  c_nn_roots_add_thunk :: CThunkPtr -> IO CInt

foreign import ccall unsafe "nn_roots_add_env"
  c_nn_roots_add_env :: Ptr NnEnv -> IO CInt

foreign import ccall unsafe "nn_roots_clear"
  c_nn_roots_clear :: IO ()

-- | Register a thunk cell as a root.
crootsAddThunk :: (MonadCData m) => CThunkPtr -> m ()
crootsAddThunk cell = cdataIO (c_nn_roots_add_thunk cell >>= registered "nn_roots_add_thunk")

-- | Register an environment as a root.
crootsAddEnv :: (MonadCData m) => Ptr NnEnv -> m ()
crootsAddEnv env = cdataIO (c_nn_roots_add_env env >>= registered "nn_roots_add_env")

-- | Forget every root (arena teardown).
crootsClear :: IO ()
crootsClear = c_nn_roots_clear

-- | Raise the failure of a registration the registry could not hold.
registered :: String -> CInt -> IO ()
registered site status = unless (status == 0) (cStatusFailure site "the root registry could not grow")
