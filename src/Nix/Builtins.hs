{-# LANGUAGE LambdaCase #-}

-- | Built-in function environment for the Nix evaluator.
--
-- Every Nix expression has access to a @builtins@ attribute set containing
-- ~100 functions.  This module assembles the initial t'Env' from the
-- builtins "Nix.Eval" defines ('Nix.Eval.listedBuiltins') and adds
-- standard constants
-- (@true@, @false@, @null@, @storeDir@, @currentTime@,
-- @currentSystem@, etc.), among them @derivation@: a Nix lambda over the
-- @derivationStrict@ primop, evaluated from its source.
-- The impure constants follow the evaluation policy: under @pure-eval@
-- upstream installs neither @currentTime@ nor @currentSystem@
-- (@addConstant@ skips an @impureOnly@ constant), so neither exists here
-- then.
--
-- The environment is allocated in the C data layer, so 'builtinEnv' runs
-- in an evaluation session ('Nix.Eval.IO.withEvalSession'); outside one
-- it raises 'Nix.Eval.CStatus.ArenaNotInitialized'.
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
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Eval (Env (..), NixValue (..), Thunk (..), attrSetFromMap, attrSetLookup, currentSystemStr, deferApply, evaluated, listedBuiltins, readThunkValue)
import Nix.Eval.AllocSite (EnvSite (EnvRoot), SetSite (SetRoot))
import Nix.Eval.Builtin (Builtin, builtinIndex, builtinName, builtinNamed)
import qualified Nix.Eval.Builtin as Builtin
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CStatus (cInvariantViolated)
import Nix.Eval.CThunk (cthunkPrimop)
import Nix.Eval.Types (AttrSet, EvalPolicy (..), cheapThunk, clistFromThunks, mkStr, mkStrBytes, newScopeEnv)
import Nix.Expr.Resolve (impureOnlyGlobalNames, staticGlobalNames)
import Nix.Parser (SourceOrigin (..), parseNixWithScope, parsedExpr)
import Nix.Store.Path (defaultStoreDirText)

-- | The initial environment: every name upstream's base environment binds
-- under the policy ('rootScopeNames'), each holding the value @builtins@
-- holds under that name less a @__@ prefix, as upstream's @addPrimOp@ and
-- @addConstant@ install one value under both (eval.cc at 2.24.9).  So
-- @__typeOf@ is @builtins.typeOf@, and @map@ is @builtins.map@.
--
-- An evaluation session builds it once ('Nix.Eval.IO.withEvalSession') and
-- evaluates every file it imports in it, as upstream's @createBaseEnv@
-- builds one base environment per @EvalState@; the @derivation@ wrapper
-- in it is the session's one.
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
builtinEnv :: (MonadCData m) => EvalPolicy -> Int64 -> [Thunk] -> m Env
builtinEnv policy timestamp searchPaths = do
  builtinsSet <- builtinsAttrSet policy timestamp searchPaths
  let bind name
        | name == "builtins" = evaluated (VAttrs builtinsSet)
        | otherwise = do
            let unprefixed = fromMaybe name (T.stripPrefix "__" name)
            attrSetLookup unprefixed builtinsSet >>= maybe (unlisted unprefixed) pure
      unlisted unprefixed = case builtinNamed unprefixed of
        Just builtin -> builtinFunction builtin
        Nothing -> cdataIO (cInvariantViolated "builtinEnv" ("the root name " ++ show unprefixed ++ " is neither in builtins nor a builtin"))
  scope <- sequenceA (Map.fromSet bind (rootScopeNames policy)) >>= attrSetFromMap SetRoot
  newScopeEnv EnvRoot Nothing scope

-- | The names the root environment binds under a policy: upstream's base
-- environment, which leaves out its @impureOnly@ constants under
-- @pure-eval@.  Source evaluated in 'builtinEnv' is bound against these.
rootScopeNames :: EvalPolicy -> Set Text
rootScopeNames policy
  | epPureEval policy = staticGlobalNames `Set.difference` impureOnlyGlobalNames
  | otherwise = staticGlobalNames

-- | The value of a builtin function bound under its own name: the
-- builtin's shared cell, so every environment 'builtinEnv' builds binds
-- the same one, as upstream binds one value per primop in its base
-- environment (@EvalState::addPrimOp@, eval.cc at 2.24.9).  A builtin
-- read in one file is then the same value as the same builtin read in
-- another, which @==@ shows of sets copied out of @builtins@ in two
-- files.  The cell is a C static ('Nix.Eval.CThunk.cthunkPrimop'), so it
-- is the same in every arena window.
builtinFunction :: (MonadCData m) => Builtin -> m Thunk
builtinFunction = fmap Thunk . cthunkPrimop . builtinIndex

-- | A base environment with a set's names bound over it, each to its
-- value in the set, as @scopedImport@ binds its scope.  The set is the
-- environment's name table, as upstream binds the scope's own values in
-- the environment it evaluates the file in (@import@ in primops.cc at
-- 2.24.9, @scopedImport@ there at 2.36.0).
builtinEnvWithScope :: (MonadCData m) => Env -> AttrSet -> m Env
builtinEnvWithScope base = newScopeEnv EnvRoot (Just base)

-- | The @builtins@ attribute set: every builtin it lists and the constants.
builtinsAttrSet :: (MonadCData m) => EvalPolicy -> Int64 -> [Thunk] -> m AttrSet
builtinsAttrSet policy timestamp searchPaths = do
  standard <- standardEntries policy timestamp searchPaths
  functions <- sequenceA (Map.fromList [(builtinName builtin, builtinFunction builtin) | builtin <- listedBuiltins])
  attrSetFromMap SetRoot (Map.union functions standard)

-- | The constants beside the builtin functions.  @currentTime@ and @currentSystem@
-- are upstream's @impureOnly@ constants and are absent under @pure-eval@
-- (@builtins ? currentTime@ is @false@ there, observed from
-- nix-instantiate 2.33.2); @nixPath@ stays, holding whatever search path
-- the caller passed, which under @pure-eval@ is what upstream leaves of
-- it.
standardEntries :: (MonadCData m) => EvalPolicy -> Int64 -> [Thunk] -> m (Map.Map Text Thunk)
standardEntries policy timestamp searchPaths =
  fmap Map.fromList . traverse sequenceA $
    [ ("true", evaluated (VBool True)),
      ("false", evaluated (VBool False)),
      ("null", evaluated VNull),
      -- Canonical, not platform: eval-visible store paths carry the
      -- /nix/store spelling on every platform, and storeDir must agree
      -- with them (upstream returns the store dir its paths are under).
      ("storeDir", evaluated (mkStr defaultStoreDirText)),
      ("nixVersion", evaluated (mkStr targetNixVersion)),
      ("langVersion", evaluated (VInt 6)),
      ("nixPath", clistFromThunks (map unThunk searchPaths) >>= evaluated . VList),
      ("derivation", derivationWrapper)
    ]
      ++ [("currentTime", evaluated (VInt timestamp)) | not (epPureEval policy)]
      ++ [("currentSystem", evaluated (mkStr currentSystemStr)) | not (epPureEval policy)]

-- | The Nix release whose behaviour the evaluator matches, reported as
-- @builtins.nixVersion@.  Upstream reports its own build's version
-- (@nixVersion@, globals.cc at 2.24.9), so an expression that branches on
-- the value takes the branch it takes on that release.
targetNixVersion :: Text
targetNixVersion = "2.24.9"

-- ---------------------------------------------------------------------------
-- The derivation constant
-- ---------------------------------------------------------------------------

-- | The source of @derivation@, a lambda over the @derivationStrict@ primop.
-- Its value is the set for the first output: the arguments, then one
-- attribute per output holding that output's set, then @all@ (the output
-- sets in the order @outputs@ lists them) and @drvAttrs@ (the arguments),
-- then the output's own @type@, @outputName@, @drvPath@ and @outPath@, a
-- later layer's attribute replacing an earlier one's of the same name.
-- @derivationStrict@ runs only when a @drvPath@ or @outPath@ is forced, so
-- forcing a derivation to WHNF forces the argument set and @outputs@ with
-- each name in it, and none of the other attributes.
--
-- The rest is fixed by what a program can observe of upstream's wrapper,
-- which the parity job compares this one with: the formals, which
-- @functionArgs@ shows, and the name the @\@@ binds, which @toXML@ shows
-- too; @outputs@ forced by @map@, then @head@, then @listToAttrs@ over every
-- name, which decides the error a malformed list raises; and the arguments'
-- own values in the result, which @==@ shows for a function: upstream's
-- forces both sides, then takes one value as equal to itself (@eqValues@,
-- eval.cc at 2.24.9).  nova-nix's @==@ takes a shared thunk as equal
-- without forcing it, which shows more of the sharing, so the wrapper also
-- shares what upstream's does: an output's set is one value in @all@,
-- under its name and as the result, and each output has a @drvPath@ of
-- its own.  The attributes it defines are at this text's lines, which
-- differ from upstream's.  Its identifiers are upstream's (@strict@,
-- @commonAttrs@, @outputsList@ and @x@), which the evaluator interns as
-- it starts ("Nix.Eval.StartupSymbols"), so compiling it interns no name,
-- wherever in an evaluation it compiles.
derivationWrapperSource :: ByteString
derivationWrapperSource =
  BC.unlines
    [ "{ outputs ? [ \"out\" ], ... }@drvAttrs:",
      "let",
      "  strict = derivationStrict drvAttrs;",
      "  outputsList = map (outputName: {",
      "    name = outputName;",
      "    value = commonAttrs // {",
      "      type = \"derivation\";",
      "      inherit outputName;",
      "      drvPath = strict.drvPath;",
      "      outPath = builtins.getAttr outputName strict;",
      "    };",
      "  }) outputs;",
      "  commonAttrs =",
      "    drvAttrs",
      "    // builtins.listToAttrs outputsList",
      "    // {",
      "      inherit drvAttrs;",
      "      all = map (x: x.value) outputsList;",
      "    };",
      "in",
      "(builtins.head outputsList).value"
    ]

-- | Where the wrapper's source is: upstream evaluates its wrapper as the
-- file @derivation-internal.nix@ of an in-memory accessor
-- (@derivationInternal@, eval.cc at 2.24.9), and a position in this one
-- reports the same file, as its path there.
derivationWrapperOrigin :: SourceOrigin
derivationWrapperOrigin = SourceFile "/derivation-internal.nix"

-- | The bindings the wrapper's free variables resolve to.  Upstream evaluates
-- its wrapper in its base environment; this is the slice of that environment
-- the wrapper reads, so the constant needs no knot through 'builtinEnv'.
-- The wrapper is bound against these names too, so it cannot read one the
-- slice lacks.
derivationWrapperScope :: (MonadCData m) => m (Map Text Thunk)
derivationWrapperScope = do
  builtinsUsed <- sequenceA (Map.fromList [(builtinName builtin, builtinFunction builtin) | builtin <- [Builtin.GetAttr, Builtin.Head, Builtin.ListToAttrs]])
  builtinsSet <- attrSetFromMap SetRoot builtinsUsed
  sequenceA $
    Map.fromList
      [ ("derivationStrict", builtinFunction Builtin.DerivationStrict),
        ("map", builtinFunction Builtin.Map),
        ("builtins", evaluated (VAttrs builtinsSet))
      ]

-- | The @derivation@ constant: the wrapper lambda closed over
-- 'derivationWrapperScope'.  One value bound in both the root scope and
-- @builtins@ of the base environment it is made for, as upstream's
-- @addConstant@ binds it.  The source is a constant, so a parse failure
-- is a defect in this module; it surfaces as an abort at the first use of
-- @derivation@ rather than as a crash while the environment is assembled.
derivationWrapper :: (MonadCData m) => m Thunk
derivationWrapper = do
  scope <- derivationWrapperScope
  env <- attrSetFromMap SetRoot scope >>= newScopeEnv EnvRoot Nothing
  case parseNixWithScope (Map.keysSet scope) "/" derivationWrapperOrigin derivationWrapperSource of
    Right parsed -> cheapThunk env (parsedExpr parsed)
    Left err -> abortingThunk ("the derivation wrapper does not parse: " <> T.pack (show err))

-- | A thunk that aborts evaluation with the message when forced: @abort@
-- applied to it, deferred the way "Nix.Eval" defers any application.
abortingThunk :: (MonadCData m) => Text -> m Thunk
abortingThunk message = evaluated (mkStr message) >>= deferApply (VBuiltin Builtin.Abort [])

-- ---------------------------------------------------------------------------
-- NIX_PATH parsing
-- ---------------------------------------------------------------------------

-- | Parse a @NIX_PATH@-formatted string into a list of search path entry
-- thunks.  Each entry becomes a @{ prefix, path }@ attrset.
--
-- Format: colon-separated entries, each either @name=path@ or plain @path@,
-- split at the first @=@.  A plain path gets an empty prefix (matching
-- real Nix behaviour).  The prefix and path keep the entry's bytes, as
-- upstream Nix keeps them.
--
-- @parseNixPath "nixpkgs=\/home\/user\/nixpkgs:custom=\/opt\/custom"@ gives
-- two evaluated thunks, @{ prefix = "nixpkgs"; path =
-- "\/home\/user\/nixpkgs"; }@ and @{ prefix = "custom"; path =
-- "\/opt\/custom"; }@.
parseNixPath :: (MonadCData m) => ByteString -> m [Thunk]
parseNixPath raw
  | BS.null raw = pure []
  | otherwise = mapM parseEntry (splitNixPath raw)
  where
    parseEntry entry = do
      let (prefix, path) = case BC.break (== '=') entry of
            (before, after)
              | BS.null after -> ("", before)
              | otherwise -> (before, BS.drop 1 after)
      prefixThunk <- evaluated (mkStrBytes prefix)
      pathThunk <- evaluated (mkStrBytes path)
      attrs <- attrSetFromMap SetRoot (Map.fromList [("prefix", prefixThunk), ("path", pathThunk)])
      evaluated (VAttrs attrs)

-- | The @path@ of every search path entry 'parseNixPath' produced, for the
-- restricted-mode allow list: upstream allows every lookup path root
-- (@resolveLookupPathPath@ with @initAccessControl@), so the roots are
-- read back from the very list the evaluator will search.  A root is the
-- bytes the entry holds.  An entry that is not an attribute set with a
-- string or path @path@ contributes nothing.
searchPathRoots :: (MonadCData m) => [Thunk] -> m [ByteString]
searchPathRoots = fmap catMaybes . mapM entryRoot
  where
    entryRoot thunk =
      readThunkValue thunk >>= \case
        Just (VAttrs attrs) ->
          attrSetLookup "path" attrs >>= \case
            Just pathThunk -> (>>= rootBytes) <$> readThunkValue pathThunk
            Nothing -> pure Nothing
        _ -> pure Nothing
    rootBytes (VStr bytes _) = Just bytes
    rootBytes (VPath p) = Just (TE.encodeUtf8 p)
    rootBytes _ = Nothing

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
