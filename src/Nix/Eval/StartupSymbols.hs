-- | The names upstream's evaluator interns as it starts, in the order it
-- interns them, which the C data layer interns before anything else as
-- the outermost evaluation session sets it up
-- ('Nix.Eval.IO.withEvalSession').
--
-- A set holds its attributes in symbol order, and a symbol's place is the
-- order its name was interned in, so that order decides which failure
-- wins wherever a walk over a set stops at the first, such as the
-- unexpected argument a call names.  Upstream's @EvalState@ interns these
-- names before it parses anything (nix-instantiate 2.24.9 with its
-- default settings): its static symbols as it is constructed, then each
-- name @createBaseEnv@ installs, then the names of the @derivation@
-- wrapper, which @createBaseEnv@ parses last.
module Nix.Eval.StartupSymbols
  ( startupSymbols,
  )
where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

-- | Every name upstream's evaluator interns as it starts, in order,
-- repeats included: interning a name already in the table changes
-- nothing.
--
-- The base environment's names are those of the default settings:
-- impure, so the two @impureOnly@ constants are there (@addConstant@,
-- eval.cc:466-485 at 2.24.9), which @pure-eval@ leaves out; no
-- experimental feature; no native code.  Interning them under
-- @pure-eval@ as well places those four names before a program's where
-- upstream's interns them with the program.
startupSymbols :: [Text]
startupSymbols =
  staticSymbols
    ++ concatMap installedNames baseEnvNames
    ++ derivationWrapperNames

-- | The symbols @EvalState@'s constructor interns, in the order its
-- members are declared (eval.hh:173-187 at 2.24.9; eval.cc:225-277), the
-- order C++ initializes them in: its @sWith@ to @sOutputSpecified@, then
-- its @exprSymbols@.
staticSymbols :: [Text]
staticSymbols =
  [ "<with>",
    "outPath",
    "drvPath",
    "type",
    "meta",
    "name",
    "value",
    "system",
    "__overrides",
    "outputs",
    "outputName",
    "__ignoreNulls",
    "file",
    "line",
    "column",
    "__functor",
    "__toString",
    "right",
    "wrong",
    "__structuredAttrs",
    "allowedReferences",
    "allowedRequisites",
    "disallowedReferences",
    "disallowedRequisites",
    "maxSize",
    "maxClosureSize",
    "builder",
    "args",
    "__contentAddressed",
    "__impure",
    "outputHash",
    "outputHashAlgo",
    "outputHashMode",
    "recurseForDerivations",
    "description",
    "self",
    "",
    "startSet",
    "operator",
    "key",
    "path",
    "prefix",
    "outputSpecified",
    "__sub",
    "__lessThan",
    "__mul",
    "__div",
    "or",
    "__findFile",
    "__nixPath",
    "body"
  ]

-- | What @addConstant@ and @addPrimOp@ intern for a name they install: the
-- name, then the name less a @__@ prefix, the one @builtins@ holds it
-- under (eval.cc:466-485 and :523-549 at 2.24.9).
installedNames :: Text -> [Text]
installedNames name = name : maybeToList (T.stripPrefix "__" name)

-- | The names @createBaseEnv@ installs, in order (primops.cc:4598-4873 at
-- 2.24.9): its constants, @__traceVerbose@ and @__nixPath@, whose
-- entries' @path@ and @prefix@ are static symbols; every registered
-- primop not gated on an experimental feature; then @derivation@.
--
-- The primops register as their translation units initialize, each in
-- the order it defines them: primops.cc, then the files of primops/ in
-- name order, the order 2.24.9's build links them (@libexpr_SOURCES@,
-- src/libexpr/local.mk).  2.35.0 compiles them in that order too, as one
-- unity build of the files its meson.build lists.  The release builds of
-- 2.27.0 through 2.34 link them under GCC's link-time optimization, which
-- runs the files' initializers in the reverse order, primops.cc's last.
-- An experimental feature leaves out @__outputOf@
-- (dynamic-derivations), @__fetchClosure@ (fetch-closure) and @fetchTree@
-- (fetch-tree, which flakes implies), and the flake primops register
-- only with flakes on.
baseEnvNames :: [Text]
baseEnvNames =
  [ "builtins",
    "true",
    "false",
    "null",
    "__currentTime",
    "__currentSystem",
    "__nixVersion",
    "__storeDir",
    "__langVersion",
    "__traceVerbose",
    "__nixPath"
  ]
    ++ primopsCc
    ++ contextCc
    ++ ["fetchMercurial"]
    ++ ["__fetchurl", "fetchTarball", "fetchGit"]
    ++ ["fromTOML"]
    ++ ["derivation"]
  where
    -- primops.cc:249-4580 at 2.24.9.
    primopsCc =
      [ "scopedImport",
        "import",
        "__typeOf",
        "isNull",
        "__isFunction",
        "__isInt",
        "__isFloat",
        "__isString",
        "__isBool",
        "__isPath",
        "__genericClosure",
        "break",
        "abort",
        "throw",
        "__addErrorContext",
        "__ceil",
        "__floor",
        "__tryEval",
        "__getEnv",
        "__seq",
        "__deepSeq",
        "__trace",
        "__warn",
        "derivationStrict",
        "placeholder",
        "__toPath",
        "__storePath",
        "__pathExists",
        "baseNameOf",
        "dirOf",
        "__readFile",
        "__findFile",
        "__hashFile",
        "__readFileType",
        "__readDir",
        "__toXML",
        "__toJSON",
        "__fromJSON",
        "__toFile",
        "__filterSource",
        "__path",
        "__attrNames",
        "__attrValues",
        "__getAttr",
        "__unsafeGetAttrPos",
        "__hasAttr",
        "__isAttrs",
        "removeAttrs",
        "__listToAttrs",
        "__intersectAttrs",
        "__catAttrs",
        "__functionArgs",
        "__mapAttrs",
        "__zipAttrsWith",
        "__isList",
        "__elemAt",
        "__head",
        "__tail",
        "map",
        "__filter",
        "__elem",
        "__concatLists",
        "__length",
        "__foldl'",
        "__any",
        "__all",
        "__genList",
        "__sort",
        "__partition",
        "__groupBy",
        "__concatMap",
        "__add",
        "__sub",
        "__mul",
        "__div",
        "__bitAnd",
        "__bitOr",
        "__bitXor",
        "__lessThan",
        "toString",
        "__substring",
        "__stringLength",
        "__hashString",
        "__convertHash",
        "__match",
        "__split",
        "__concatStringsSep",
        "__replaceStrings",
        "__parseDrvName",
        "__compareVersions",
        "__splitVersion"
      ]
    -- primops/context.cc at 2.24.9.
    contextCc =
      [ "__unsafeDiscardStringContext",
        "__hasContext",
        "__unsafeDiscardOutputDependency",
        "__addDrvOutputDependencies",
        "__getContext",
        "__appendContext"
      ]

-- | The names upstream's @derivation@ wrapper (primops/derivation.nix at
-- 2.24.9) brings that nothing above interns, in the order its parser
-- interns them (parser.y:179-483): @strict@ and @drvAttrs@ from its first
-- binding, @commonAttrs@ and @outputsList@ from its second, @x@ from that
-- binding's @x: x.value@, then @outputToAttrListElement@.  Every other
-- name the wrapper reads is a static symbol or one the base environment
-- installs.  @createBaseEnv@ evaluates the file once it has installed
-- everything (primops.cc:4872).  This evaluator's own wrapper
-- ("Nix.Builtins") names its values with these names, so compiling it
-- interns none.
derivationWrapperNames :: [Text]
derivationWrapperNames =
  [ "strict",
    "drvAttrs",
    "commonAttrs",
    "outputsList",
    "x",
    "outputToAttrListElement"
  ]
