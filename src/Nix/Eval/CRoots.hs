-- | The roots of an evaluation session in the C data layer
-- (@cbits\/nn_roots.h@): each C object Haskell keeps beyond one
-- evaluation step, registered as it is made, so a mark over the C graph
-- starts from it without the caller naming it ("Nix.Eval.CStats").  A
-- session registers its base environment and search path as it starts,
-- and each imported file's value as the import cache takes it
-- ("Nix.Eval.IO").  A session forgets the roots registered within it as
-- it ends ('crootsMark', 'crootsRelease'), and the arena's teardown
-- forgets them all.
module Nix.Eval.CRoots
  ( crootsAddThunk,
    crootsAddEnv,
    RootsMark,
    crootsMark,
    crootsRelease,
    crootsClear,
  )
where

import Control.Monad (unless)
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CEnv (NnEnv)
import Nix.Eval.CStatus (cStatusFailure)
import Nix.Eval.CThunk (CThunkPtr)

foreign import ccall unsafe "nn_roots_add_thunk"
  c_nn_roots_add_thunk :: CThunkPtr -> IO CInt

foreign import ccall unsafe "nn_roots_add_env"
  c_nn_roots_add_env :: Ptr NnEnv -> IO CInt

foreign import ccall unsafe "nn_roots_mark"
  c_nn_roots_mark :: Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "nn_roots_release"
  c_nn_roots_release :: Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_roots_clear"
  c_nn_roots_clear :: IO ()

-- | The registry's size at a point, to release back to.
data RootsMark = RootsMark !Word32 !Word32

-- | The registry's size now.
crootsMark :: IO RootsMark
crootsMark =
  alloca $ \thunks -> alloca $ \envs -> do
    c_nn_roots_mark thunks envs
    RootsMark <$> peek thunks <*> peek envs

-- | Forget every root registered since the mark.
crootsRelease :: RootsMark -> IO ()
crootsRelease (RootsMark thunks envs) = c_nn_roots_release thunks envs

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
