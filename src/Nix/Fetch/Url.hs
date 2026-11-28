{-# LANGUAGE LambdaCase #-}

-- | URLs as upstream's fetchers read and write them: parsed into a
-- scheme, an authority, a path of decoded segments, a query and a
-- fragment, and rendered back with the percent-encoding upstream applies
-- (url.cc at 2.36.0).
--
-- Upstream parses with Boost.URL's grammar for a URI reference (RFC
-- 3986), so a string with a character the grammar has no place for is not
-- a URL, and a flake reference falls back to being a path.  Its lenient
-- mode, the one flake references use, first percent-encodes the spaces
-- and double quotes in a query and the spaces, double quotes and carets
-- in a fragment, which older releases let through unencoded.
--
-- A URL is bytes: a path segment may decode to any byte, so every part is
-- held as a 'ByteString', and a message spells one through
-- 'bytesToTextLossy'.  Every function here is pure; a warning a parse
-- gives is returned with its result ('Checked').
module Nix.Fetch.Url
  ( -- * Checked results
    Checked,
    runChecked,
    refuse,
    warn,

    -- * URLs
    ParsedUrl (..),
    Authority (..),
    HostType (..),
    Leniency (..),
    parseUrl,
    renderUrl,
    renderAuthority,
    urlPathSegments,
    emptyAuthority,

    -- * Schemes
    UrlScheme (..),
    parseUrlScheme,

    -- * Encoding
    percentEncode,
    percentDecode,
    decodeQuery,
    encodeQuery,

    -- * Git URLs
    fixGitUrl,

    -- * Paths
    pathToUrlPath,
    urlPathToPath,
    isAbsoluteHostPath,

    -- * Messages
    bytesToTextLossy,
  )
where

import Control.Monad (forM, unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit, toLower)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word16, Word8)
import Nix.Eval.Policy (PathRules (..))
import Numeric (showHex)

-- ---------------------------------------------------------------------------
-- Checked results
-- ---------------------------------------------------------------------------

-- | A step that may refuse its input, with the warnings it gave in the
-- order it gave them.  The warnings outlive a refusal, as upstream prints
-- each one when it is given, before any later error.
type Checked = ExceptT Text (Writer [Text])

-- | The outcome of a checked step and the warnings it gave.
runChecked :: Checked a -> (Either Text a, [Text])
runChecked = runWriter . runExceptT

-- | Refuse, with upstream's message.
refuse :: Text -> Checked a
refuse = throwError

-- | Give a warning, upstream's text without its @warning:@ prefix.
warn :: Text -> Checked ()
warn message = tell [message]

-- | Bytes as text for a message, a byte with no UTF-8 reading replaced.
bytesToTextLossy :: ByteString -> Text
bytesToTextLossy = TE.decodeUtf8With lenientDecode

-- | A message's quoting of bytes.
quoted :: ByteString -> Text
quoted bytes = "'" <> bytesToTextLossy bytes <> "'"

-- ---------------------------------------------------------------------------
-- URLs
-- ---------------------------------------------------------------------------

-- | A URL as upstream's @ParsedURL@ holds it: the path as its segments,
-- each decoded, a leading empty one standing for a leading @\/@; the
-- query's parameters, each decoded, by name; and the fragment, decoded,
-- empty when there is none.
data ParsedUrl = ParsedUrl
  { urlScheme :: !ByteString,
    urlAuthority :: !(Maybe Authority),
    urlPath :: ![ByteString],
    urlQuery :: !(Map ByteString ByteString),
    urlFragment :: !ByteString
  }
  deriving (Eq, Show)

-- | A URL's authority, its parts decoded.
data Authority = Authority
  { authHostType :: !HostType,
    authHost :: !ByteString,
    authUser :: !(Maybe ByteString),
    authPassword :: !(Maybe ByteString),
    authPort :: !(Maybe Word16)
  }
  deriving (Eq, Show)

-- | What kind of host an authority names, which decides how it renders.
data HostType = HostName | HostIPv4 | HostIPv6 | HostIPvFuture
  deriving (Eq, Show)

-- | An authority with an empty host, as @file:\/\/\/@ has.
emptyAuthority :: Authority
emptyAuthority = Authority HostName BS.empty Nothing Nothing Nothing

-- | Whether a parse first encodes the characters older releases left
-- unencoded in a query and a fragment.
data Leniency = Lenient | Strict
  deriving (Eq, Show)

-- | The path's segments less the empty ones (@ParsedURL::pathSegments@
-- with @skipEmpty@).
urlPathSegments :: ParsedUrl -> [ByteString]
urlPathSegments = filter (not . BS.null) . urlPath

-- | Parse a URL as upstream's @parseURL@ does: a URI reference that
-- must have a scheme, its authority, path, query and fragment decoded.
-- A file URL whose authority names a host is refused, and one with no
-- path has the path @\/@.
parseUrl :: Leniency -> ByteString -> Checked ParsedUrl
parseUrl leniency url = case splitReference buffer of
  Nothing -> refuse (quoted url <> " is not a valid URL: " <> grammarRefusal)
  Just reference -> case refScheme reference of
    Nothing -> refuse (quoted buffer <> " doesn't have a scheme")
    Just scheme -> do
      authority <- traverse parseAuthority (refAuthority reference)
      let transportIsFile = utTransport (parseUrlScheme scheme) == fileScheme
      case authority of
        Just parsed
          | not (BS.null (authHost parsed)) && transportIsFile ->
              refuse ("file:// URL " <> quoted buffer <> " has unexpected authority " <> quoted (renderAuthority parsed))
        _ -> pure ()
      let encodedPath
            | transportIsFile && BS.null (refPath reference) = BC.singleton '/'
            | otherwise = refPath reference
      -- Upstream's splitString keeps an empty string as one empty
      -- segment, where 'BC.split' gives none.
      path <- traverse percentDecode (if BS.null encodedPath then [BS.empty] else BC.split '/' encodedPath)
      query <- decodeQuery leniency (fromMaybe BS.empty (refQuery reference))
      fragment <- percentDecode (fromMaybe BS.empty (refFragment reference))
      pure (ParsedUrl scheme authority path query fragment)
  where
    buffer = case leniency of
      Lenient -> lenientFixup url
      Strict -> url

-- | How upstream words a URL its grammar refuses: Boost.URL's own reason,
-- which for a character the grammar has no place for is input left over.
grammarRefusal :: Text
grammarRefusal = "leftover"

-- | Upstream's lenient fixup: the query's spaces and double quotes, and
-- the fragment's spaces, double quotes and carets, percent-encoded.  The
-- query is what follows the first @?@ up to the next @#@, and the
-- fragment what follows the first @#@ after the query, or the first @#@
-- when there is no query.
lenientFixup :: ByteString -> ByteString
lenientFixup url = case BC.break (== '?') url of
  (beforeQuery, rest)
    | Just afterMark <- BC.stripPrefix "?" rest ->
        let (queryPart, fromFragment) = BC.break (== '#') afterMark
         in beforeQuery <> "?" <> encodeChars queryExtras queryPart <> fixFragment fromFragment
  _ -> fixFragment url
  where
    fixFragment view = case BC.break (== '#') view of
      (beforeFragment, rest)
        | Just fragment <- BC.stripPrefix "#" rest -> beforeFragment <> "#" <> encodeChars fragmentExtras fragment
      _ -> view
    encodeChars extras = BC.concatMap (\c -> if c `elem` extras then encodedByte (charByte c) else BC.singleton c)

-- | The characters older releases accepted unencoded in a query.
queryExtras :: String
queryExtras = " \""

-- | The characters older releases accepted unencoded in a fragment.
fragmentExtras :: String
fragmentExtras = " \"^"

-- | A file URL's transport.
fileScheme :: ByteString
fileScheme = "file"

-- | Render a URL as upstream's @ParsedURL::to_string@ does: each path
-- segment encoded keeping @:@ and @\@@, the query's names and values
-- keeping @:@, @\@@, @\/@ and @?@, and the fragment keeping only the
-- unreserved characters.
renderUrl :: ParsedUrl -> ByteString
renderUrl url =
  BS.concat
    [ urlScheme url,
      ":",
      maybe BS.empty (("//" <>) . renderAuthority) (urlAuthority url),
      encodeUrlPath (urlPath url),
      if Map.null (urlQuery url) then BS.empty else "?" <> encodeQuery (urlQuery url),
      if BS.null (urlFragment url) then BS.empty else "#" <> percentEncode BS.empty (urlFragment url)
    ]

-- | Render an authority as upstream's does: the user and password
-- encoded, a host name encoded, an IPv4 address as it is, and an IPv6
-- address in brackets.
renderAuthority :: Authority -> ByteString
renderAuthority authority =
  BS.concat
    [ case authUser authority of
        Just user -> percentEncode BS.empty user <> maybe BS.empty ((":" <>) . percentEncode BS.empty) (authPassword authority) <> "@"
        Nothing -> BS.empty,
      case authHostType authority of
        HostName -> percentEncode BS.empty (authHost authority)
        HostIPv4 -> authHost authority
        HostIPv6 -> "[" <> percentEncode ":" (authHost authority) <> "]"
        HostIPvFuture -> "[" <> percentEncode ":" (authHost authority) <> "]",
      maybe BS.empty ((":" <>) . BC.pack . show) (authPort authority)
    ]

-- | A path's segments encoded and joined with @\/@.
encodeUrlPath :: [ByteString] -> ByteString
encodeUrlPath = BS.intercalate "/" . map (percentEncode pathKeeps)

-- | What a path segment keeps unencoded beyond the unreserved characters.
pathKeeps :: ByteString
pathKeeps = ":@"

-- | What a query keeps unencoded beyond the unreserved characters.
queryKeeps :: ByteString
queryKeeps = ":@/?"

-- | A query's parameters encoded, in name order, as upstream's
-- @encodeQuery@ writes them.
encodeQuery :: Map ByteString ByteString -> ByteString
encodeQuery query =
  BS.intercalate "&" [percentEncode queryKeeps name <> "=" <> percentEncode queryKeeps value | (name, value) <- Map.toAscList query]

-- ---------------------------------------------------------------------------
-- Schemes
-- ---------------------------------------------------------------------------

-- | A scheme split as upstream's @parseUrlScheme@ splits it: an
-- application before the first @+@, and the transport after it.
data UrlScheme = UrlScheme
  { utApplication :: !(Maybe ByteString),
    utTransport :: !ByteString
  }
  deriving (Eq, Show)

-- | Split a scheme into its application and transport.
parseUrlScheme :: ByteString -> UrlScheme
parseUrlScheme scheme = case BC.break (== '+') scheme of
  (application, rest) | Just transport <- BC.stripPrefix "+" rest -> UrlScheme (Just application) transport
  _ -> UrlScheme Nothing scheme

-- ---------------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------------

-- | Encode every byte but the unreserved characters and those kept as
-- @%XX@, upper-case, as Boost.URL's @encode@ does.
percentEncode :: ByteString -> ByteString -> ByteString
percentEncode keep = BC.concatMap encodeOne
  where
    encodeOne c
      | isUnreserved c || BC.elem c keep = BC.singleton c
      | otherwise = encodedByte (charByte c)

-- | A byte as @%XX@.
encodedByte :: Word8 -> ByteString
encodedByte byte = BC.pack ['%', hexDigit (byte `shiftR` 4), hexDigit (byte .&. 0xf)]
  where
    hexDigit nibble
      | nibble < 10 = toEnum (fromEnum '0' + fromIntegral nibble)
      | otherwise = toEnum (fromEnum 'A' + fromIntegral nibble - 10)

-- | A character's byte.
charByte :: Char -> Word8
charByte = fromIntegral . fromEnum

-- | Decode every @%XX@, refusing a @%@ not followed by two hex digits as
-- upstream's @percentDecode@ does.
percentDecode :: ByteString -> Checked ByteString
percentDecode encoded = maybe (refuse ("invalid URI parameter " <> quoted encoded <> ": " <> pctRefusal)) pure (decodeEncoded encoded)

-- | How Boost.URL words a @%@ without two hex digits.
pctRefusal :: Text
pctRefusal = "bad hexdig"

-- | Decode every @%XX@, or 'Nothing' for a @%@ without two hex digits.
decodeEncoded :: ByteString -> Maybe ByteString
decodeEncoded = fmap BS.pack . go . BS.unpack
  where
    go bytes = case bytes of
      [] -> Just []
      percent : hi : lo : rest
        | percent == percentByte,
          Just high <- hexValue hi,
          Just low <- hexValue lo ->
            ((high * 16 + low) :) <$> go rest
      percent : _ | percent == percentByte -> Nothing
      byte : rest -> (byte :) <$> go rest
    percentByte = charByte '%'

-- | A hex digit's value.
hexValue :: Word8 -> Maybe Word8
hexValue byte
  | isHexDigit c = Just (fromIntegral (hexDigitValue c))
  | otherwise = Nothing
  where
    c = toEnum (fromIntegral byte)
    hexDigitValue d
      | isDigit d = fromEnum d - fromEnum '0'
      | otherwise = fromEnum (toLower d) - fromEnum 'a' + 10

-- | Decode a query as upstream's @decodeQuery@ does: leniently, its
-- spaces and double quotes encoded first; then each @&@-separated
-- parameter split at its first @=@ and decoded, the first of two with one
-- name kept, and one with no @=@ dropped with a warning.  A query the
-- grammar refuses is refused.
decodeQuery :: Leniency -> ByteString -> Checked (Map ByteString ByteString)
decodeQuery leniency query
  | not (validEncoded isQueryChar fixed) = refuse ("invalid URI query " <> quoted query <> ": " <> grammarRefusal)
  | BS.null fixed = pure Map.empty
  | otherwise = do
      params <- forM (BC.split '&' fixed) $ \param -> case BC.break (== '=') param of
        (name, rest)
          | Just value <- BC.stripPrefix "=" rest -> Just <$> ((,) <$> percentDecode name <*> percentDecode value)
          | otherwise -> Nothing <$ warn ("dubious URI query " <> quoted name <> " is missing equal sign '=', ignoring")
      pure (Map.fromListWith (\_ earlier -> earlier) (catMaybes params))
  where
    fixed = case leniency of
      Lenient -> BC.concatMap (\c -> if c `elem` queryExtras then encodedByte (charByte c) else BC.singleton c) query
      Strict -> query

-- ---------------------------------------------------------------------------
-- The grammar
-- ---------------------------------------------------------------------------

-- | A URI reference split into its parts, each still encoded.
data Reference = Reference
  { refScheme :: !(Maybe ByteString),
    refAuthority :: !(Maybe ByteString),
    refPath :: !ByteString,
    refQuery :: !(Maybe ByteString),
    refFragment :: !(Maybe ByteString)
  }

-- | Split a string as RFC 3986's URI reference, or 'Nothing' when the
-- grammar refuses it: a URI when it starts with a scheme and a colon, a
-- relative reference otherwise, whose first segment then holds no colon.
splitReference :: ByteString -> Maybe Reference
splitReference input = do
  let (scheme, afterScheme) = case BC.uncons input of
        Just (first, _)
          | isAlpha first,
            (name, rest) <- BC.span isSchemeChar input,
            Just hier <- BC.stripPrefix ":" rest ->
              (Just name, hier)
        _ -> (Nothing, input)
      (beforeFragment, fragment) = splitAtMark '#' afterScheme
      (hierPart, query) = splitAtMark '?' beforeFragment
      (authority, path) = case BC.stripPrefix "//" hierPart of
        Just afterSlashes -> let (auth, rest) = BC.break (== '/') afterSlashes in (Just auth, rest)
        Nothing -> (Nothing, hierPart)
  unless (all (validEncoded isFragmentChar) fragment && all (validEncoded isQueryChar) query) Nothing
  unless (validEncoded (\c -> isPathChar c || c == '/') path) Nothing
  when (isJust authority && not (BS.null path || BC.isPrefixOf "/" path)) Nothing
  -- A relative reference's first segment cannot hold a colon, which would
  -- read as a scheme.
  when (isNothing scheme && isNothing authority && BC.elem ':' (BC.takeWhile (/= '/') path)) Nothing
  mapM_ splitAuthority authority
  pure (Reference scheme authority path query fragment)
  where
    splitAtMark mark s = case BC.break (== mark) s of
      (before, rest) | Just after <- BC.stripPrefix (BC.singleton mark) rest -> (before, Just after)
      _ -> (s, Nothing)

-- | An authority split into its encoded user information, its host and
-- the host's kind, and its port's digits, or 'Nothing' when the grammar
-- refuses it.
data AuthorityParts = AuthorityParts
  { apUserInfo :: !(Maybe ByteString),
    apHostType :: !HostType,
    apHost :: !ByteString,
    apPort :: !(Maybe ByteString)
  }

splitAuthority :: ByteString -> Maybe AuthorityParts
splitAuthority authority = do
  let (userInfo, hostPort) = case BC.break (== '@') authority of
        (user, rest) | Just after <- BC.stripPrefix "@" rest -> (Just user, after)
        _ -> (Nothing, authority)
  unless (all (validEncoded (\c -> isUnreserved c || isSubDelim c || c == ':')) userInfo) Nothing
  (hostType, host, portPart) <- case BC.stripPrefix "[" hostPort of
    Just inner -> do
      let (literal, rest) = BC.break (== ']') inner
      afterBracket <- BC.stripPrefix "]" rest
      kind <- literalKind literal
      pure (kind, literal, afterBracket)
    Nothing -> do
      let (name, rest) = BC.break (== ':') hostPort
      unless (validEncoded (\c -> isUnreserved c || isSubDelim c) name) Nothing
      pure (if isJust (parseIPv4 name) then HostIPv4 else HostName, name, rest)
  port <- case BC.uncons portPart of
    Nothing -> Just Nothing
    Just (':', digits) | BC.all isDigit digits -> Just (Just digits)
    _ -> Nothing
  pure (AuthorityParts userInfo hostType host port)
  where
    literalKind literal
      | isJust (parseIPv6 literal) = Just HostIPv6
      | isIPvFuture literal = Just HostIPvFuture
      | otherwise = Nothing

-- | An authority as upstream's @Authority::parse@ reads one: its parts
-- decoded, a user and a password when the user information holds them,
-- and a port only when it is a number from 1 to 65535.
parseAuthority :: ByteString -> Checked Authority
parseAuthority encoded = case splitAuthority encoded of
  Nothing -> refuse ("invalid URL authority: " <> quoted encoded <> ": " <> grammarRefusal)
  Just parts -> do
    host <- case apHostType parts of
      HostName -> percentDecode (apHost parts)
      _ -> pure (apHost parts)
    (user, password) <- case apUserInfo parts of
      Nothing -> pure (Nothing, Nothing)
      Just info -> case BC.break (== ':') info of
        (name, rest)
          | Just secret <- BC.stripPrefix ":" rest -> (\decoded decodedSecret -> (Just decoded, Just decodedSecret)) <$> percentDecode name <*> percentDecode secret
          | otherwise -> (\decoded -> (Just decoded, Nothing)) <$> percentDecode name
    port <- case apPort parts of
      Nothing -> pure Nothing
      Just digits
        | BS.null digits -> pure Nothing
        | Just number <- portNumber digits -> pure (Just number)
        | otherwise -> refuse ("port " <> quoted digits <> " is invalid")
    pure (Authority (apHostType parts) host user password port)
  where
    portNumber digits = case BC.readInteger digits of
      Just (n, rest) | BS.null rest, n >= 1, n <= toInteger (maxBound :: Word16) -> Just (fromInteger n)
      _ -> Nothing

-- | Whether every character is one the class allows, or part of a
-- @%XX@.
validEncoded :: (Char -> Bool) -> ByteString -> Bool
validEncoded allowed = go
  where
    go s = case BC.uncons s of
      Nothing -> True
      Just ('%', rest) -> case BC.unpack (BC.take 2 rest) of
        [hi, lo] | isHexDigit hi && isHexDigit lo -> go (BS.drop 2 rest)
        _ -> False
      Just (c, rest) -> allowed c && go rest

isAlpha :: Char -> Bool
isAlpha c = isAsciiLower c || isAsciiUpper c

isSchemeChar :: Char -> Bool
isSchemeChar c = isAlpha c || isDigit c || c `elem` ("+-." :: String)

isUnreserved :: Char -> Bool
isUnreserved c = isAlpha c || isDigit c || c `elem` ("-._~" :: String)

isSubDelim :: Char -> Bool
isSubDelim c = c `elem` ("!$&'()*+,;=" :: String)

isPathChar :: Char -> Bool
isPathChar c = isUnreserved c || isSubDelim c || c == ':' || c == '@'

isQueryChar :: Char -> Bool
isQueryChar c = isPathChar c || c == '/' || c == '?'

isFragmentChar :: Char -> Bool
isFragmentChar = isQueryChar

-- | An IPv4 address's four octets, each 0 to 255 with no leading zero.
parseIPv4 :: ByteString -> Maybe [Int]
parseIPv4 s = case BC.split '.' s of
  octets@[_, _, _, _] -> traverse decOctet octets
  _ -> Nothing
  where
    decOctet o
      | BS.null o || BS.length o > 3 || not (BC.all isDigit o) = Nothing
      | BS.length o > 1 && BC.isPrefixOf "0" o = Nothing
      | otherwise = case BC.readInt o of
          Just (n, _) | n <= 255 -> Just n
          _ -> Nothing

-- | An IPv6 address's eight 16-bit groups, read as RFC 3986's
-- @IPv6address@: groups of one to four hex digits, at most one @::@
-- standing for one or more zero groups, and an IPv4 address in place of
-- the last two.
parseIPv6 :: ByteString -> Maybe [Int]
parseIPv6 s = case breakDouble s of
  Just (before, after) -> do
    left <- groupsOf False before
    right <- groupsOf True after
    let missing = 8 - length left - length right
    if missing >= 1 then Just (left ++ replicate missing 0 ++ right) else Nothing
  Nothing -> do
    groups <- groupsOf True s
    if length groups == 8 then Just groups else Nothing
  where
    breakDouble text = case BS.breakSubstring "::" text of
      (before, rest)
        | not (BS.null rest),
          after <- BS.drop 2 rest,
          not ("::" `BS.isInfixOf` after) ->
            Just (before, after)
      _ -> Nothing
    groupsOf ipv4Last text
      | BS.null text = Just []
      | otherwise = do
          let parts = BC.split ':' text
          lastGroups <- case reverse parts of
            final : _
              | ipv4Last,
                BC.elem '.' final -> do
                  octets <- parseIPv4 final
                  case octets of
                    [a, b, c, d] -> Just [a * 256 + b, c * 256 + d]
                    _ -> Nothing
            final : _ -> pure <$> h16 final
            [] -> Nothing
          initial <- traverse h16 (dropEnd1 parts)
          Just (initial ++ lastGroups)
    h16 g
      | BS.null g || BS.length g > 4 || not (BC.all isHexDigit g) = Nothing
      | otherwise = Just (BC.foldl' (\acc c -> acc * 16 + hexDigitInt c) 0 g)
    dropEnd1 xs = take (length xs - 1) xs
    hexDigitInt c
      | isDigit c = fromEnum c - fromEnum '0'
      | otherwise = fromEnum (toLower c) - fromEnum 'a' + 10

-- | Whether a literal is RFC 3986's @IPvFuture@: @v@, hex digits, a dot,
-- then unreserved characters, sub-delimiters and colons.
isIPvFuture :: ByteString -> Bool
isIPvFuture literal = case BC.uncons literal of
  Just (v, rest) | toLower v == 'v' -> case BC.break (== '.') rest of
    (version, dotted)
      | not (BS.null version),
        BC.all isHexDigit version,
        Just afterDot <- BC.stripPrefix "." dotted ->
          not (BS.null afterDot) && BC.all (\c -> isUnreserved c || isSubDelim c || c == ':') afterDot
    _ -> False
  _ -> False

-- | An IPv6 address as Boost.URL renders one (RFC 5952): each group in
-- lower-case hex without leading zeros, the longest run of two or more
-- zero groups, the first of equals, written as @::@.
renderIPv6 :: [Int] -> ByteString
renderIPv6 groups = BC.pack $ case longestZeroRun of
  Just (start, len) ->
    intercalate ":" (map hexGroup (take start groups)) ++ "::" ++ intercalate ":" (map hexGroup (drop (start + len) groups))
  Nothing -> intercalate ":" (map hexGroup groups)
  where
    hexGroup g = showHex g ""
    indexed = zip [0 :: Int ..] groups
    -- Each run of zero groups, as its first index and its length.
    runs = zeroRuns indexed
    zeroRuns entries = case dropWhile ((/= 0) . snd) entries of
      [] -> []
      zeros@((start, _) : _) ->
        let (run, rest) = span ((== 0) . snd) zeros
         in (start, length run) : zeroRuns rest
    longestZeroRun = foldl' pickLonger Nothing (filter ((>= 2) . snd) runs)
    pickLonger best run = case best of
      Just (_, bestLen) | bestLen >= snd run -> best
      _ -> Just run

-- ---------------------------------------------------------------------------
-- Git URLs
-- ---------------------------------------------------------------------------

-- | A Git URL as upstream's @fixGitURL@ makes one: an absolute path
-- becomes a @file:\/\/\/@ URL, its bytes taken as they are; an SCP-style
-- @host:path@ becomes an @ssh:\/\/@ URL; anything else is parsed strictly,
-- and a @git+@ prefix dropped from its scheme.
fixGitUrl :: PathRules -> ByteString -> Checked ParsedUrl
fixGitUrl rules url
  | isAbsoluteHostPath rules url = pure (ParsedUrl fileScheme (Just emptyAuthority) (pathToUrlPath rules url) Map.empty BS.empty)
  | otherwise =
      scpStyle url >>= \case
        Just parsed -> pure parsed
        Nothing -> do
          parsed <- parseUrl Strict url
          pure $ case parseUrlScheme (urlScheme parsed) of
            UrlScheme (Just "git") transport -> parsed {urlScheme = transport}
            _ -> parsed

-- | The schemes Git reads as such, with and without @git+@: a URL with
-- one of them before its first colon is no SCP-style URL.
schemesSupportedByGit :: [ByteString]
schemesSupportedByGit = concat [[scheme, "git+" <> scheme] | scheme <- ["ssh", "http", "https", "file", "ftp", "ftps", "git"]]

-- | An SCP-style Git URL as upstream's @tryParseScpStyle@ reads one, or
-- 'Nothing' when the URL is not one: it holds no @:\/\/@, has a colon with
-- no @\/@ before it, does not start with a scheme Git knows, and a
-- bracketed host closes with @]@ right before the colon.  A relative path
-- is made absolute with a warning.
scpStyle :: ByteString -> Checked (Maybe ParsedUrl)
scpStyle url
  | "://" `BS.isInfixOf` url = pure Nothing
  | otherwise = case BC.elemIndex ':' url of
      Nothing -> pure Nothing
      Just firstColon
        | BC.elem '/' (BS.take firstColon url) -> pure Nothing
        | BS.take firstColon url `elem` schemesSupportedByGit -> pure Nothing
        | otherwise -> case bracketedHostEnd of
            BracketNotScp -> pure Nothing
            BracketHostEnds hostEnd -> Just <$> scpParts (BS.take hostEnd url) (BS.drop (hostEnd + 1) url)
            NoBracket -> Just <$> scpParts (BS.take firstColon url) (BS.drop (firstColon + 1) url)
  where
    bracketStart = case BS.breakSubstring "@[" url of
      (before, rest) | not (BS.null rest) -> Just (BS.length before + 1)
      _ -> if BC.isPrefixOf "[" url then Just 0 else Nothing
    bracketedHostEnd = case bracketStart of
      Nothing -> NoBracket
      Just start -> case BC.elemIndex ']' (BS.drop (start + 1) url) of
        Nothing -> NoBracket
        Just offset ->
          let close = start + 1 + offset
           in case BS.indexMaybe url (close + 1) of
                Just next | next == charByte ':' -> BracketHostEnds (close + 1)
                _ -> BracketNotScp
    scpParts hostWithUser pathView = do
      let (user, host) = case BC.break (== '@') hostWithUser of
            (name, rest) | Just after <- BC.stripPrefix "@" rest -> (Just name, after)
            _ -> (Nothing, hostWithUser)
      authority <- case () of
        _
          | Just octets <- parseIPv4 host -> pure (Authority HostIPv4 (BC.pack (intercalate "." (map show octets))) user Nothing Nothing)
          | Just inner <- BC.stripPrefix "[" host >>= BC.stripSuffix "]" -> case parseIPv6 inner of
              Just groups -> pure (Authority HostIPv6 (renderIPv6 groups) user Nothing Nothing)
              Nothing -> refuse ("Git SCP bracketed URL is not valid: " <> quoted inner <> " is not a valid IPv6 address")
          | otherwise -> pure (Authority HostName host user Nothing Nothing)
      when (BS.null pathView) $ refuse ("SCP-style Git URL " <> quoted url <> " has an empty path")
      let segments = BC.split '/' pathView
          parsed = ParsedUrl "ssh" (Just authority) segments Map.empty BS.empty
      case segments of
        first : _ | not (BS.null first) -> do
          let absolute = parsed {urlPath = BS.empty : segments}
              tilde = absolute {urlPath = BS.empty : "~" : segments}
          unless ("~" `BS.isPrefixOf` first) $
            warn
              ( "SCP-style URL "
                  <> quoted url
                  <> " has a relative path which cannot be faithfully converted to an SSH URL: we will convert it to an absolute path when making an SSH URL.\n\nUse an explicit absolute path:\n\n    "
                  <> bytesToTextLossy (host <> ":/" <> pathView)
                  <> "\n\nor with an explicit SSH URL:\n\n    "
                  <> bytesToTextLossy (renderUrl absolute)
                  <> "\n\ninstead, to explicitly use an absolute path (and silence this warning). Or, to be relative to the remote user's home directory:\n\n    "
                  <> bytesToTextLossy (host <> ":~/" <> pathView)
                  <> "\n\nor with an explicit SSH URL:\n\n    "
                  <> bytesToTextLossy (renderUrl tilde)
              )
          pure absolute
        _ -> pure parsed

-- | Where an SCP-style URL's bracketed host ends, when it has one.
data BracketedHost = NoBracket | BracketHostEnds !Int | BracketNotScp

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

-- | Whether a path is absolute as @std::filesystem::path@ reads one on
-- the host: rooted at @\/@ on POSIX, and with a drive and a separator
-- after it on Windows.
isAbsoluteHostPath :: PathRules -> ByteString -> Bool
isAbsoluteHostPath rules path = case rules of
  PosixPaths -> BC.isPrefixOf "/" path
  WindowsPaths -> case BC.unpack (BS.take 3 path) of
    [drive, ':', sep] -> isAlpha drive && isSeparator WindowsPaths sep
    _ -> False

isSeparator :: PathRules -> Char -> Bool
isSeparator rules c = c == '/' || (rules == WindowsPaths && c == '\\')

-- | A path's URL segments as upstream's @pathToUrlPath@ makes them: an
-- empty segment for a root directory, a drive as its own segment, then
-- the path's elements as @std::filesystem::path@ iterates them, separators
-- collapsed and a trailing one an empty element, and one more empty
-- segment when the path has no file name, as one ending in a separator
-- has not.
pathToUrlPath :: PathRules -> ByteString -> [ByteString]
pathToUrlPath rules path = root ++ drive ++ elements ++ [BS.empty | BS.null path || endsWithSeparator]
  where
    (driveName, afterDrive) = case (rules, BC.unpack (BS.take 2 path)) of
      (WindowsPaths, [letter, ':']) | isAlpha letter -> (Just (BS.take 2 path), BS.drop 2 path)
      _ -> (Nothing, path)
    rooted = maybe False (isSeparator rules . fst) (BC.uncons afterDrive)
    root = [BS.empty | rooted]
    drive = maybe [] pure driveName
    relative = BC.dropWhile (isSeparator rules) afterDrive
    pieces = filter (not . BS.null) (BC.splitWith (isSeparator rules) relative)
    endsWithSeparator = maybe False (isSeparator rules . snd) (BC.unsnoc path)
    elements = pieces ++ [BS.empty | not (BS.null relative) && endsWithSeparator]

-- | A path from URL segments as upstream's @urlPathToPath@ makes it: a
-- segment holding a @\/@ or a NUL is refused; a leading empty segment
-- roots it (at the drive the next segment names, on Windows); and each
-- further segment is appended as @std::filesystem::path@'s @\/=@ appends
-- it, an empty one adding a separator only after a file name.
urlPathToPath :: PathRules -> [ByteString] -> Checked ByteString
urlPathToPath rules segments = do
  mapM_ checkSegment segments
  pure $ case segments of
    [] -> BS.empty
    first : rest
      | BS.null first -> case (rules, rest) of
          (WindowsPaths, driveSegment : more) | isDrive driveSegment -> foldl' append (driveSegment <> "/") more
          _ -> foldl' append "/" rest
      | otherwise -> foldl' append BS.empty segments
  where
    checkSegment segment
      | BC.elem '/' segment = refuse ("URL path component " <> quoted segment <> " contains '/', which is not allowed in file names")
      | BC.elem '\0' segment = refuse ("URL path component " <> quoted (BC.concatMap (\c -> if c == '\0' then nulSymbol else BC.singleton c) segment) <> " contains NUL byte which is not allowed")
      | otherwise = pure ()
    isDrive segment = case BC.unpack segment of
      [letter, ':'] -> isAlpha letter
      _ -> False
    append acc segment
      | BS.null segment = if hasFileName acc then acc <> "/" else acc
      | BS.null acc = segment
      | hasFileName acc = acc <> "/" <> segment
      | otherwise = acc <> segment
    hasFileName acc = maybe False (not . isSeparator rules . snd) (BC.unsnoc acc)
    -- U+2400, SYMBOL FOR NULL, in UTF-8, as upstream spells a NUL in this
    -- message.
    nulSymbol = BS.pack [0xe2, 0x90, 0x80]
