-- | A builtin's partial application in the C data layer
-- (@cbits\/nn_primop.h@): the builtin's index, the cells of the arguments
-- it has taken, and which of them were taken forced.  A thunk holds one
-- under @NN_VALUE_PRIMOP_APP@, so a partial application, like every
-- other value, lives in C and holds nothing in GHC's heap.
--
-- Applications are tracked and freed in bulk at arena teardown, or one by
-- one by the region collector ("Nix.Eval.CRegion") once nothing reaches
-- them.
module Nix.Eval.CPrimop
  ( -- * Opaque handle
    NnPrimopApp,
    CPrimopAppPtr,

    -- * Lifecycle
    cprimopAppNew,

    -- * Accessors
    cprimopAppPrimop,
    cprimopAppTaken,
    cprimopAppForced,
    cprimopAppArg,
  )
where

import Data.Word (Word16, Word32)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CStatus (checkedCPtr)
import Nix.Eval.CThunk (CThunkPtr)

-- | Phantom type for C-side @nn_primop_app_t@.
data NnPrimopApp

-- | Pointer to a C-allocated partial application.
type CPrimopAppPtr = Ptr NnPrimopApp

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_primop_app_new"
  c_nn_primop_app_new :: Word32 -> Word16 -> Ptr CThunkPtr -> Word16 -> IO CPrimopAppPtr

foreign import ccall unsafe "nn_primop_app_primop"
  c_nn_primop_app_primop :: CPrimopAppPtr -> IO Word32

foreign import ccall unsafe "nn_primop_app_taken"
  c_nn_primop_app_taken :: CPrimopAppPtr -> IO Word16

foreign import ccall unsafe "nn_primop_app_forced"
  c_nn_primop_app_forced :: CPrimopAppPtr -> IO Word16

foreign import ccall unsafe "nn_primop_app_arg"
  c_nn_primop_app_arg :: CPrimopAppPtr -> Word16 -> IO CThunkPtr

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | A partial application of the builtin at an index to the cells of the
-- arguments it has taken, first to last, with bit @i@ of the mask set
-- when argument @i@ was taken forced.  No argument, or more than the
-- mask has bits, aborts in C; a partial application never has either.
cprimopAppNew :: (MonadCData m) => Word32 -> Word16 -> [CThunkPtr] -> m CPrimopAppPtr
cprimopAppNew primop forced args =
  cdataIO $ withArrayLen args $ \taken argsPtr ->
    checkedCPtr "nn_primop_app_new" =<< c_nn_primop_app_new primop forced argsPtr (fromIntegral taken)

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | The builtin's index.
cprimopAppPrimop :: (MonadCData m) => CPrimopAppPtr -> m Word32
cprimopAppPrimop = cdataIO . c_nn_primop_app_primop

-- | How many arguments it has taken.
cprimopAppTaken :: (MonadCData m) => CPrimopAppPtr -> m Word16
cprimopAppTaken = cdataIO . c_nn_primop_app_taken

-- | Which arguments were taken forced, bit @i@ for argument @i@.
cprimopAppForced :: (MonadCData m) => CPrimopAppPtr -> m Word16
cprimopAppForced = cdataIO . c_nn_primop_app_forced

-- | Argument @i@'s cell.  @i@ is below 'cprimopAppTaken': an index past
-- it aborts (@NN_ASSERT@ in @nn_primop_app_arg@).
cprimopAppArg :: (MonadCData m) => CPrimopAppPtr -> Word16 -> m CThunkPtr
cprimopAppArg app i = cdataIO (c_nn_primop_app_arg app i)
