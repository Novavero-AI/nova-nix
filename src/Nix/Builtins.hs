-- | Built-in function environment for the Nix evaluator.
--
-- Every Nix expression has access to a @builtins@ attribute set containing
-- ~100 functions.  This module assembles the initial 'Env' from the
-- central registry in "Nix.Eval" and adds standard constants
-- (@true@, @false@, @null@, @storeDir@, @currentTime@,
-- @currentSystem@, etc.), among them @derivation@: upstream's wrapper
-- lambda around the @derivationStrict@ primop, evaluated from its source.
-- The impure constants follow the evaluation policy: under @pure-eval@
-- upstream installs neither @currentTime@ nor @currentSystem@
-- (@addConstant@ skips an @impureOnly@ constant), so neither exists here
-- then.
--
-- The environment is allocated in the C data layer, so 'builtinEnv' must
-- be forced between 'Nix.Eval.Arena.arenaInit' and
-- 'Nix.Eval.Arena.arenaDestroy'; outside that window forcing it raises
-- 'Nix.Eval.CStatus.ArenaNotInitialized'.
module Nix.Builtins
  ( -- * Builtin registration
    builtinEnv,
    builtinEnvWithScope,
    rootScopeNames,

    -- * NIX_PATH parsing
    parseNixPath,
    splitNixPath,
    searchPathRoots,
    isNixPathPseudoUrl,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.Ptr (nullPtr)
import Nix.Eval (Env (..), NixValue (..), Thunk (..), attrSetFromMap, attrSetLookup, builtinNames, currentSystemStr, deferApply, evaluated, readThunkValue)
import Nix.Eval.Types (AttrSet, EvalPolicy (..), cheapThunk, clistFromThunks, mkStr, mkStrBytes, newCEnv, thunkToCPtr)
import Nix.Expr.Resolve (impureOnlyGlobalNames, staticGlobalNames)
import Nix.Parser (parseNixWithScope)
import Nix.Store.Path (defaultStoreDirText)

-- | The initial environment: every name upstream's base environment binds
-- under the policy ('rootScopeNames'), each holding the value @builtins@
-- holds under that name less a @__@ prefix, as upstream's @addPrimOp@ and
-- @addConstant@ install one value under both (eval.cc at 2.24.9).  So
-- @__typeOf@ is @builtins.typeOf@, and @map@ is @builtins.map@.
--
-- @fetchMercurial@ is the one name upstream binds by default that nova-nix
-- does not implement: it is bound as a builtin of that name, so binding
-- accepts it and a call fails with the evaluator's unknown-builtin error.
-- @builtins@ does not list it, so feature tests still see it is missing.
--
-- @currentTime@ is an integer constant (seconds since epoch),
-- passed in at startup.  In tests, pass @0@.
--
-- @searchPaths@ populates @builtins.nixPath@.  Parsed from @NIX_PATH@
-- by 'parseNixPath'.  In tests, pass @[]@.
--
-- The policy decides which constants exist: see 'standardEntries'.
builtinEnv :: EvalPolicy -> Int64 -> [Thunk] -> Env
builtinEnv policy timestamp searchPaths =
  let builtinsSet = builtinsAttrSet policy timestamp searchPaths
      bind name
        | name == "builtins" = evaluated (VAttrs builtinsSet)
        | otherwise =
            let unprefixed = fromMaybe name (T.stripPrefix "__" name)
             in fromMaybe (evaluated (VBuiltin unprefixed [])) (attrSetLookup unprefixed builtinsSet)
      scope = attrSetFromMap (Map.fromSet bind (rootScopeNames policy))
   in newCEnv nullPtr 0 (Just scope) Nothing nullPtr 0

-- | The names the root environment binds under a policy: upstream's base
-- environment, which leaves out its @impureOnly@ constants under
-- @pure-eval@.  Source evaluated in 'builtinEnv' is bound against these.
rootScopeNames :: EvalPolicy -> Set Text
rootScopeNames policy
  | epPureEval policy = staticGlobalNames `Set.difference` impureOnlyGlobalNames
  | otherwise = staticGlobalNames

-- | Create a top-level binding for a builtin function.
topLevelBuiltin :: Text -> (Text, Thunk)
topLevelBuiltin name = (name, evaluated (VBuiltin name []))

-- | Like 'builtinEnv' but with additional scope bindings overlaid on
-- the top-level environment.  Used by @scopedImport@.
builtinEnvWithScope :: EvalPolicy -> Int64 -> [Thunk] -> [(Text, Thunk)] -> Env
builtinEnvWithScope policy timestamp searchPaths scope =
  let base = builtinEnv policy timestamp searchPaths
      scopeMap = Map.fromList scope
   in newCEnv nullPtr 0 (Just (attrSetFromMap scopeMap)) (Just base) nullPtr 0

-- | The @builtins@ attribute set, derived from the central registry.
builtinsAttrSet :: EvalPolicy -> Int64 -> [Thunk] -> AttrSet
builtinsAttrSet policy timestamp searchPaths =
  attrSetFromMap $ Map.union builtinEntries (standardEntries policy timestamp searchPaths)
  where
    builtinEntries =
      Map.fromList [(name, evaluated (VBuiltin name [])) | name <- builtinNames]

-- | The constants beside the registry.  @currentTime@ and @currentSystem@
-- are upstream's @impureOnly@ constants and are absent under @pure-eval@
-- (@builtins ? currentTime@ is @false@ there, observed from
-- nix-instantiate 2.33.2); @nixPath@ stays, holding whatever search path
-- the caller passed, which under @pure-eval@ is what upstream leaves of
-- it.
standardEntries :: EvalPolicy -> Int64 -> [Thunk] -> Map.Map Text Thunk
standardEntries policy timestamp searchPaths =
  Map.fromList $
    [ ("true", evaluated (VBool True)),
      ("false", evaluated (VBool False)),
      ("null", evaluated VNull),
      -- Canonical, not platform: eval-visible store paths carry the
      -- /nix/store spelling on every platform, and storeDir must agree
      -- with them (upstream returns the store dir its paths are under).
      ("storeDir", evaluated (mkStr defaultStoreDirText)),
      ("nixVersion", evaluated (mkStr "2.24.0")),
      ("langVersion", evaluated (VInt 6)),
      ("nixPath", evaluated (VList (clistFromThunks (map thunkToCPtr searchPaths)))),
      ("derivation", derivationWrapper)
    ]
      ++ [("currentTime", evaluated (VInt timestamp)) | not (epPureEval policy)]
      ++ [("currentSystem", evaluated (mkStr currentSystemStr)) | not (epPureEval policy)]

-- ---------------------------------------------------------------------------
-- The derivation constant
-- ---------------------------------------------------------------------------

-- | Upstream's @src\/libexpr\/primops\/derivation.nix@ at 2.24.9, without its
-- documentation comment.  @derivation@ is this lambda, not a primop, so the
-- result set has upstream's shape by construction: every output attribute
-- is the complete derivation set for that output, @all@ lists them,
-- @drvAttrs@ is the argument set, and @derivationStrict@ is reached only
-- through @outPath@ and @drvPath@, which is what keeps forcing a derivation
-- to WHNF from forcing any of its inputs.
derivationWrapperSource :: Text
derivationWrapperSource =
  T.unlines
    [ "drvAttrs @ { outputs ? [ \"out\" ], ... }:",
      "",
      "let",
      "",
      "  strict = derivationStrict drvAttrs;",
      "",
      "  commonAttrs = drvAttrs // (builtins.listToAttrs outputsList) //",
      "    { all = map (x: x.value) outputsList;",
      "      inherit drvAttrs;",
      "    };",
      "",
      "  outputToAttrListElement = outputName:",
      "    { name = outputName;",
      "      value = commonAttrs // {",
      "        outPath = builtins.getAttr outputName strict;",
      "        drvPath = strict.drvPath;",
      "        type = \"derivation\";",
      "        inherit outputName;",
      "      };",
      "    };",
      "",
      "  outputsList = map outputToAttrListElement outputs;",
      "",
      "in (builtins.head outputsList).value"
    ]

-- | The name upstream gives the wrapper's source in error positions.
derivationWrapperName :: Text
derivationWrapperName = "derivation-internal.nix"

-- | The bindings the wrapper's free variables resolve to.  Upstream evaluates
-- the file in its base environment; this is the slice of that environment
-- the wrapper reads, so the constant needs no knot through 'builtinEnv'.
-- The wrapper is bound against these names too, so it cannot read one the
-- slice lacks.
derivationWrapperScope :: Map Text Thunk
derivationWrapperScope =
  let builtinsUsed = ["getAttr", "head", "listToAttrs"]
   in Map.fromList
        [ ("derivationStrict", evaluated (VBuiltin "derivationStrict" [])),
          ("map", evaluated (VBuiltin "map" [])),
          ("builtins", evaluated (VAttrs (attrSetFromMap (Map.fromList (map topLevelBuiltin builtinsUsed)))))
        ]
{-# NOINLINE derivationWrapperScope #-}

-- | 'derivationWrapperScope' as the wrapper's environment.
derivationWrapperEnv :: Env
derivationWrapperEnv =
  newCEnv nullPtr 0 (Just (attrSetFromMap derivationWrapperScope)) Nothing nullPtr 0
{-# NOINLINE derivationWrapperEnv #-}

-- | The @derivation@ constant: the wrapper lambda closed over
-- 'derivationWrapperEnv'.  One value bound in both the root scope and
-- @builtins@, as upstream's @addConstant@ binds it.  It is compiled once
-- for the process rather than once per environment: 'builtinEnv' runs for
-- every imported file, and the arena the lambda lives in is the process's
-- ("Nix.Eval.Arena").  The source is a constant, so a parse failure is a
-- defect in this module; it surfaces as an abort at the first use of
-- @derivation@ rather than as a crash while the environment is assembled.
derivationWrapper :: Thunk
derivationWrapper =
  case parseNixWithScope (Map.keysSet derivationWrapperScope) "/" derivationWrapperName derivationWrapperSource of
    Right expr -> cheapThunk derivationWrapperEnv expr
    Left err -> abortingThunk ("the derivation wrapper does not parse: " <> T.pack (show err))
{-# NOINLINE derivationWrapper #-}

-- | A thunk that aborts evaluation with the message when forced: @abort@
-- applied to it, deferred the way "Nix.Eval" defers any application.
abortingThunk :: Text -> Thunk
abortingThunk message = deferApply (VBuiltin "abort" []) (evaluated (mkStr message))

-- ---------------------------------------------------------------------------
-- NIX_PATH parsing
-- ---------------------------------------------------------------------------

-- | Parse a @NIX_PATH@-formatted string into a list of search path entry
-- thunks.  Each entry becomes a @{ prefix, path }@ attrset.
--
-- Format: colon-separated entries, each either @name=path@ or plain @path@,
-- split at the first @=@.  A plain path gets an empty prefix (matching
-- real Nix behaviour).  The prefix and path keep the entry's bytes, as
-- upstream's @LookupPath::Elem::parse@ keeps them
-- (src/libexpr/search-path.cc at Nix 2.24.9).
--
-- >>> parseNixPath "nixpkgs=/home/user/nixpkgs:custom=/opt/custom"
-- [Evaluated (VAttrs {"prefix": "nixpkgs", "path": "/home/user/nixpkgs"}), ...]
parseNixPath :: ByteString -> [Thunk]
parseNixPath raw
  | BS.null raw = []
  | otherwise = map parseEntry (splitNixPath raw)
  where
    parseEntry entry =
      let (prefix, path) = case BC.break (== '=') entry of
            (before, after)
              | BS.null after -> ("", before)
              | otherwise -> (before, BS.drop 1 after)
       in evaluated
            ( VAttrs
                ( attrSetFromMap $
                    Map.fromList
                      [ ("prefix", evaluated (mkStrBytes prefix)),
                        ("path", evaluated (mkStrBytes path))
                      ]
                )
            )

-- | The @path@ of every search path entry 'parseNixPath' produced, for the
-- restricted-mode allow list: upstream allows every lookup path root
-- (@resolveLookupPathPath@ with @initAccessControl@), so the roots are
-- read back from the very list the evaluator will search.  A root is the
-- bytes the entry holds.  An entry that is not an attribute set with a
-- string or path @path@ contributes nothing.
searchPathRoots :: [Thunk] -> [ByteString]
searchPathRoots = mapMaybe entryRoot
  where
    entryRoot thunk = do
      VAttrs attrs <- readThunkValue thunk
      pathThunk <- attrSetLookup "path" attrs
      pathVal <- readThunkValue pathThunk
      case pathVal of
        VStr bytes _ -> Just bytes
        VPath p -> Just (TE.encodeUtf8 p)
        _ -> Nothing

-- | URL schemes that keep a @NIX_PATH@ entry whole when @://@ follows
-- them: the allowlist in @EvalSettings::isPseudoUrl@,
-- src/libexpr/eval-settings.cc at Nix 2.24.9.
nixPathUrlSchemes :: [ByteString]
nixPathUrlSchemes = ["http", "https", "file", "channel", "git", "s3", "ssh"]

-- | Schemes that keep a @NIX_PATH@ entry whole whatever follows their
-- colon: @isPseudoUrl@'s @channel:@ test and @parseNixPath@'s @flake:@
-- test, both in src/libexpr/eval-settings.cc at Nix 2.24.9.
nixPathBareSchemes :: [ByteString]
nixPathBareSchemes = ["channel", "flake"]

-- | Whether a search path entry's path names something fetched rather
-- than a place on this filesystem: upstream's @EvalSettings::isPseudoUrl@
-- (a @channel:@ prefix, or one of 'nixPathUrlSchemes' before @://@),
-- plus the @flake:@ entries a lookup path hook resolves
-- (@resolveLookupPathPath@ in src/libexpr/eval.cc at Nix 2.24.9).
isNixPathPseudoUrl :: ByteString -> Bool
isNixPathPseudoUrl path =
  any (\bare -> (bare <> ":") `BS.isPrefixOf` path) nixPathBareSchemes
    || (not (BS.null afterScheme) && scheme `elem` nixPathUrlSchemes)
  where
    (scheme, afterScheme) = BS.breakSubstring "://" path

-- | Split a @NIX_PATH@ string into entries the way upstream's
-- @EvalSettings::parseNixPath@ does (src/libexpr/eval-settings.cc at
-- Nix 2.24.9).  An entry runs to the first colon.  That colon stays in
-- the entry, which then extends through exactly one more colon-free
-- run, when the text between the last @=@ before the colon (or the
-- entry's start) and the colon is:
--
-- * one of 'nixPathBareSchemes', whatever follows the colon, so
--   @channel:nixos-24.11@ and @nixpkgs=channel:nixos-24.11@ stay whole;
-- * one of 'nixPathUrlSchemes' with @//@ after the colon, so
--   @nixpkgs=https://example.com/nixpkgs.tar.gz@ stays whole while an
--   unlisted @foo://x@ splits into @foo@ and @//x@, as it does upstream;
-- * a single ASCII letter with @/@ or @\\@ after the colon: a Windows
--   drive colon, so @C:\\x@, @C:/x@ and @nixpkgs=C:\\x@ stay whole.
--   Upstream has no such rule and splits every drive path at its letter
--   (nix-instantiate 2.33.2 reports @C:\\a@ as the entries @C@ and @\\a@).
--   The rule also keeps a one-letter scheme such as @C://x@ whole, which
--   upstream splits into @C@ and @//x@.
--
-- Only one further run is absorbed, so @https://example.com:8080/x@
-- splits after the host exactly as it does upstream.  Every other colon
-- separates: @/foo:/bar@ is two entries.  An empty entry is dropped:
-- @a::b@ is @a@ and @b@, @::x@ is @x@, @a:@ is @a@.  Upstream's parser
-- emits a leading or interior empty entry (never a trailing one), but
-- the entries are then round-tripped through a whitespace-separated
-- setting (initGC in src/libexpr/eval-gc.cc) that drops it, so
-- @builtins.nixPath@ never shows one.  That round trip also splits an
-- entry at a space, which is not reproduced here: a Windows path may
-- contain one, the same reason the drive rule above diverges.
--
-- The split works on bytes, as upstream's does.  Every delimiter is
-- ASCII, and no byte of a multibyte UTF-8 sequence is, so a UTF-8 entry
-- splits exactly where its text would and any other byte passes through
-- unchanged.
splitNixPath :: ByteString -> [ByteString]
splitNixPath = filter (not . BS.null) . go
  where
    go remaining
      | BS.null remaining = []
      | otherwise =
          let (segment, rest) = BC.break (== ':') remaining
           in case BC.uncons rest of
                Nothing -> [segment]
                Just (_, afterColon)
                  | keepsColon (BC.takeWhileEnd (/= '=') segment) afterColon ->
                      let (absorbed, afterEntry) = BC.break (== ':') afterColon
                       in BS.concat [segment, ":", absorbed] : go (BS.drop 1 afterEntry)
                  | otherwise -> segment : go afterColon
    keepsColon scheme afterColon
      | scheme `elem` nixPathBareSchemes = True
      | scheme `elem` nixPathUrlSchemes = BS.isPrefixOf "//" afterColon
      | otherwise = isDriveLetter scheme && startsWithPathSep afterColon
    isDriveLetter scheme = case BC.uncons scheme of
      Just (letter, afterLetter) -> BS.null afterLetter && (isAsciiUpper letter || isAsciiLower letter)
      Nothing -> False
    startsWithPathSep t = case BC.uncons t of
      Just (c, _) -> c == '/' || c == '\\'
      Nothing -> False
