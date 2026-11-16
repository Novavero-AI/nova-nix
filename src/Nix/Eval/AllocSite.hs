-- | The construct or builtin behind each environment and each attribute
-- set the C data layer allocates.
--
-- The evaluator passes a site with every such allocation, and while the
-- census counts sites ("Nix.Eval.CStats") the allocators count each
-- allocation under it (@cbits\/nn_stats.h@).  With counting off a site
-- costs the argument that carries it.
module Nix.Eval.AllocSite
  ( -- * Environments
    EnvSite (..),
    envSiteIndex,
    envSiteName,

    -- * Attribute sets
    SetSite (..),
    setSiteIndex,
    setSiteName,
  )
where

import qualified Data.ByteString as BS
import Data.Word (Word32)

-- | What makes an environment.
data EnvSite
  = -- | The root environment: an evaluation's, each imported file's, a
    -- @scopedImport@'s and the @derivation@ wrapper's.
    EnvRoot
  | -- | A call of a lambda with an argument name: the argument.
    EnvCall
  | -- | A call of a lambda with formals: the formals, and the name an @\@@
    -- binds.
    EnvCallFormals
  | -- | A lambda value closed over the variables it uses, which it
    -- copies out of its scope.
    EnvClosure
  | -- | A @let@'s bindings.
    EnvLet
  | -- | The variables a @let@ uses from outside it, copied out of its
    -- scope as a closure's are.
    EnvLetCapture
  | -- | A @rec@ set's bindings.
    EnvRec
  | -- | The variables a @rec@ set uses from outside it.
    EnvRecCapture
  | -- | One name of an @inherit (from)@: the from-expression.
    EnvInheritFrom
  | -- | A @with@.
    EnvWith
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The site's number in the C counts, below @NN_STATS_ENV_SITES@.
envSiteIndex :: EnvSite -> Word32
envSiteIndex = fromIntegral . fromEnum

-- | The site's name in the census report.
envSiteName :: EnvSite -> BS.ByteString
envSiteName site = case site of
  EnvRoot -> "root"
  EnvCall -> "call"
  EnvCallFormals -> "call-formals"
  EnvClosure -> "closure"
  EnvLet -> "let"
  EnvLetCapture -> "let-capture"
  EnvRec -> "rec"
  EnvRecCapture -> "rec-capture"
  EnvInheritFrom -> "inherit-from"
  EnvWith -> "with"

-- | What makes an attribute set.
data SetSite
  = -- | The root environment's sets: @builtins@, the names around every
    -- expression, a @scopedImport@'s scope, the @derivation@ wrapper's
    -- scope, and the search path's entries.
    SetRoot
  | -- | A non-recursive attribute set literal.
    SetLiteral
  | -- | A @rec@ set.
    SetRec
  | -- | The name table of a @let@ or @rec@ set with a dynamic name.
    SetScope
  | -- | The @//@ operator.
    SetUpdate
  | -- | @listToAttrs@.
    SetListToAttrs
  | -- | @removeAttrs@.
    SetRemoveAttrs
  | -- | @intersectAttrs@.
    SetIntersectAttrs
  | -- | @mapAttrs@.
    SetMapAttrs
  | -- | @zipAttrsWith@.
    SetZipAttrsWith
  | -- | @functionArgs@.
    SetFunctionArgs
  | -- | @groupBy@.
    SetGroupBy
  | -- | @partition@.
    SetPartition
  | -- | @parseDrvName@.
    SetParseDrvName
  | -- | @tryEval@.
    SetTryEval
  | -- | @getContext@: the result and each path's entry in it.
    SetGetContext
  | -- | @fromJSON@: each object.
    SetFromJSON
  | -- | @fromTOML@: each table.
    SetFromTOML
  | -- | @readDir@.
    SetReadDir
  | -- | @fetchGit@.
    SetFetchGit
  | -- | @derivationStrict@.
    SetDerivationStrict
  | -- | A position's value, as @unsafeGetAttrPos@ and @__curPos@ make it.
    SetPosition
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The site's number in the C counts, below @NN_STATS_SET_SITES@.
setSiteIndex :: SetSite -> Word32
setSiteIndex = fromIntegral . fromEnum

-- | The site's name in the census report.
setSiteName :: SetSite -> BS.ByteString
setSiteName site = case site of
  SetRoot -> "root"
  SetLiteral -> "literal"
  SetRec -> "rec"
  SetScope -> "scope"
  SetUpdate -> "update"
  SetListToAttrs -> "listToAttrs"
  SetRemoveAttrs -> "removeAttrs"
  SetIntersectAttrs -> "intersectAttrs"
  SetMapAttrs -> "mapAttrs"
  SetZipAttrsWith -> "zipAttrsWith"
  SetFunctionArgs -> "functionArgs"
  SetGroupBy -> "groupBy"
  SetPartition -> "partition"
  SetParseDrvName -> "parseDrvName"
  SetTryEval -> "tryEval"
  SetGetContext -> "getContext"
  SetFromJSON -> "fromJSON"
  SetFromTOML -> "fromTOML"
  SetReadDir -> "readDir"
  SetFetchGit -> "fetchGit"
  SetDerivationStrict -> "derivationStrict"
  SetPosition -> "position"
