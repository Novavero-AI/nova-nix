-- | The builtins the evaluator knows: one constructor for each primop of
-- upstream's base environment that nova-nix binds.  A builtin value names
-- one ('Nix.Eval.Types.VBuiltin'), and "Nix.Eval" gives each its
-- arguments and implementation in one total definition, so a builtin
-- with no definition does not compile.  The C data layer holds a builtin
-- as its index ('builtinIndex'), its constructor's position.
--
-- Meant to be imported qualified, its constructors named as
-- @Builtin.Map@, beside the type imported unqualified.
module Nix.Eval.Builtin
  ( Builtin (..),
    builtinName,
    builtinNamed,
    builtinIndex,
    builtinAtIndex,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word32)

-- | A builtin, by its name less upstream's @__@ prefix.
data Builtin
  = Abort
  | Add
  | AddDrvOutputDependencies
  | AddErrorContext
  | All
  | Any
  | AppendContext
  | AttrNames
  | AttrValues
  | BaseNameOf
  | BitAnd
  | BitOr
  | BitXor
  | Break
  | CatAttrs
  | Ceil
  | CompareVersions
  | ConcatLists
  | ConcatMap
  | ConcatStringsSep
  | ConvertHash
  | DeepSeq
  | DerivationStrict
  | DirOf
  | Div
  | Elem
  | ElemAt
  | FetchGit
  | FetchMercurial
  | FetchTarball
  | Fetchurl
  | Filter
  | FilterSource
  | FindFile
  | Floor
  | FoldlStrict
  | FromJSON
  | FromTOML
  | FunctionArgs
  | GenList
  | GenericClosure
  | GetAttr
  | GetContext
  | GetEnv
  | GroupBy
  | HasAttr
  | HasContext
  | HashFile
  | HashString
  | Head
  | Import
  | IntersectAttrs
  | IsAttrs
  | IsBool
  | IsFloat
  | IsFunction
  | IsInt
  | IsList
  | IsNull
  | IsPath
  | IsString
  | Length
  | LessThan
  | ListToAttrs
  | Map
  | MapAttrs
  | Match
  | Mul
  | ParseDrvName
  | Partition
  | Path
  | PathExists
  | Placeholder
  | ReadDir
  | ReadFile
  | ReadFileType
  | RemoveAttrs
  | ReplaceStrings
  | ScopedImport
  | Seq
  | Sort
  | Split
  | SplitVersion
  | StorePath
  | StringLength
  | Sub
  | Substring
  | Tail
  | Throw
  | ToFile
  | ToJSON
  | ToPath
  | ToString
  | ToXML
  | Trace
  | TraceVerbose
  | TryEval
  | TypeOf
  | UnsafeDiscardOutputDependency
  | UnsafeDiscardStringContext
  | UnsafeGetAttrPos
  | Warn
  | ZipAttrsWith
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The name a builtin is bound under in @builtins@.
builtinName :: Builtin -> Text
builtinName builtin = case builtin of
  Abort -> "abort"
  Add -> "add"
  AddDrvOutputDependencies -> "addDrvOutputDependencies"
  AddErrorContext -> "addErrorContext"
  All -> "all"
  Any -> "any"
  AppendContext -> "appendContext"
  AttrNames -> "attrNames"
  AttrValues -> "attrValues"
  BaseNameOf -> "baseNameOf"
  BitAnd -> "bitAnd"
  BitOr -> "bitOr"
  BitXor -> "bitXor"
  Break -> "break"
  CatAttrs -> "catAttrs"
  Ceil -> "ceil"
  CompareVersions -> "compareVersions"
  ConcatLists -> "concatLists"
  ConcatMap -> "concatMap"
  ConcatStringsSep -> "concatStringsSep"
  ConvertHash -> "convertHash"
  DeepSeq -> "deepSeq"
  DerivationStrict -> "derivationStrict"
  DirOf -> "dirOf"
  Div -> "div"
  Elem -> "elem"
  ElemAt -> "elemAt"
  FetchGit -> "fetchGit"
  FetchMercurial -> "fetchMercurial"
  FetchTarball -> "fetchTarball"
  Fetchurl -> "fetchurl"
  Filter -> "filter"
  FilterSource -> "filterSource"
  FindFile -> "findFile"
  Floor -> "floor"
  FoldlStrict -> "foldl'"
  FromJSON -> "fromJSON"
  FromTOML -> "fromTOML"
  FunctionArgs -> "functionArgs"
  GenList -> "genList"
  GenericClosure -> "genericClosure"
  GetAttr -> "getAttr"
  GetContext -> "getContext"
  GetEnv -> "getEnv"
  GroupBy -> "groupBy"
  HasAttr -> "hasAttr"
  HasContext -> "hasContext"
  HashFile -> "hashFile"
  HashString -> "hashString"
  Head -> "head"
  Import -> "import"
  IntersectAttrs -> "intersectAttrs"
  IsAttrs -> "isAttrs"
  IsBool -> "isBool"
  IsFloat -> "isFloat"
  IsFunction -> "isFunction"
  IsInt -> "isInt"
  IsList -> "isList"
  IsNull -> "isNull"
  IsPath -> "isPath"
  IsString -> "isString"
  Length -> "length"
  LessThan -> "lessThan"
  ListToAttrs -> "listToAttrs"
  Map -> "map"
  MapAttrs -> "mapAttrs"
  Match -> "match"
  Mul -> "mul"
  ParseDrvName -> "parseDrvName"
  Partition -> "partition"
  Path -> "path"
  PathExists -> "pathExists"
  Placeholder -> "placeholder"
  ReadDir -> "readDir"
  ReadFile -> "readFile"
  ReadFileType -> "readFileType"
  RemoveAttrs -> "removeAttrs"
  ReplaceStrings -> "replaceStrings"
  ScopedImport -> "scopedImport"
  Seq -> "seq"
  Sort -> "sort"
  Split -> "split"
  SplitVersion -> "splitVersion"
  StorePath -> "storePath"
  StringLength -> "stringLength"
  Sub -> "sub"
  Substring -> "substring"
  Tail -> "tail"
  Throw -> "throw"
  ToFile -> "toFile"
  ToJSON -> "toJSON"
  ToPath -> "toPath"
  ToString -> "toString"
  ToXML -> "toXML"
  Trace -> "trace"
  TraceVerbose -> "traceVerbose"
  TryEval -> "tryEval"
  TypeOf -> "typeOf"
  UnsafeDiscardOutputDependency -> "unsafeDiscardOutputDependency"
  UnsafeDiscardStringContext -> "unsafeDiscardStringContext"
  UnsafeGetAttrPos -> "unsafeGetAttrPos"
  Warn -> "warn"
  ZipAttrsWith -> "zipAttrsWith"

-- | The builtin bound under a name, if any.
builtinNamed :: Text -> Maybe Builtin
builtinNamed name = Map.lookup name byName

-- | Every builtin by its name.
byName :: Map Text Builtin
byName = Map.fromList [(builtinName builtin, builtin) | builtin <- [minBound .. maxBound]]

-- | A builtin's index in the C data layer: its constructor's position.
builtinIndex :: Builtin -> Word32
builtinIndex = fromIntegral . fromEnum

-- | The builtin at an index, if any.
builtinAtIndex :: Word32 -> Maybe Builtin
builtinAtIndex index = Map.lookup index byIndex

-- | Every builtin by its index.
byIndex :: Map Word32 Builtin
byIndex = Map.fromList [(builtinIndex builtin, builtin) | builtin <- [minBound .. maxBound]]
