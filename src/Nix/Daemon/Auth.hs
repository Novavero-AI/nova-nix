{-# LANGUAGE LambdaCase #-}

-- | Who may connect to a daemon, and whom it trusts, as upstream's daemon
-- decides from what the operating system says of a connection's peer:
-- its user, matched against @trusted-users@ and @allowed-users@ by name,
-- by @*@, or by a group named with @\@@; and its group, a member of the
-- build users' group being refused.
--
-- The decision is pure over the user database it is given, a
-- t'UserDatabase', so it runs in tests over users that do not exist;
-- "Nix.Daemon.Listen" gives it the system's, and a peer's identity read
-- from a Unix socket.
module Nix.Daemon.Auth
  ( -- * Peers
    Peer (..),
    unknownPeer,

    -- * The user database
    UserDatabase (..),
    GroupEntry (..),

    -- * Admission
    AuthSettings (..),
    Admission (..),
    admitPeer,
    acceptedMessage,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Nix.Daemon.Protocol (Trust (..))

-- ---------------------------------------------------------------------------
-- Peers
-- ---------------------------------------------------------------------------

-- | What the operating system says of a connection's peer, as far as it
-- says: its process, its user and its group, each by number.
data Peer = Peer
  { peerProcess :: !(Maybe Word32),
    peerUser :: !(Maybe Word32),
    peerGroup :: !(Maybe Word32)
  }
  deriving (Eq, Show)

-- | A peer the daemon knows nothing of, as one whose trust is forced is
-- treated: its identity is not read.
unknownPeer :: Peer
unknownPeer = Peer Nothing Nothing Nothing

-- ---------------------------------------------------------------------------
-- The user database
-- ---------------------------------------------------------------------------

-- | A group as the user database records it: its number and the names
-- it lists as members.
data GroupEntry = GroupEntry
  { groupEntryId :: !Word32,
    groupEntryMembers :: ![ByteString]
  }
  deriving (Eq, Show)

-- | What the decision asks of the system's users and groups.
data UserDatabase m = UserDatabase
  { -- | A user's name, by number.
    udUserName :: !(Word32 -> m (Maybe ByteString)),
    -- | A group's name, by number.
    udGroupName :: !(Word32 -> m (Maybe ByteString)),
    -- | A group, by name.
    udGroup :: !(ByteString -> m (Maybe GroupEntry)),
    -- | Whether a user is a member of a group, by their numbers, where
    -- the system answers that apart from a group's member list, as
    -- macOS's directory does, and 'Nothing' where it does not or the
    -- question fails.
    udMembership :: !(Word32 -> Word32 -> m (Maybe Bool))
  }

-- ---------------------------------------------------------------------------
-- Admission
-- ---------------------------------------------------------------------------

-- | The daemon's settings that decide admission, as bytes, matched
-- against the user database's.
data AuthSettings = AuthSettings
  { -- | @trusted-users@.
    asTrustedUsers :: ![ByteString],
    -- | @allowed-users@.
    asAllowedUsers :: ![ByteString],
    -- | @build-users-group@, empty for none.
    asBuildUsersGroup :: !ByteString
  }
  deriving (Eq, Show)

-- | What becomes of a peer.
data Admission
  = -- | It may connect, trusted or not, and is named so, where it has a
    -- name.
    Admitted !Trust !(Maybe ByteString)
  | -- | It may not, for this reason.
    Refused !Text
  deriving (Eq, Show)

-- | Decide a peer's admission as upstream's daemon does.  The peer is
-- named by its user's name, or its user's number where the user has no
-- entry, and its group likewise.  It is trusted when @trusted-users@
-- matches it, and a peer that is not is refused unless @allowed-users@
-- matches it; a peer whose group is the build users' is refused either
-- way.
admitPeer :: (Monad m) => UserDatabase m -> AuthSettings -> Peer -> m Admission
admitPeer db settings peer = do
  user <- traverse (\uid -> fromMaybe (decimal uid) <$> udUserName db uid) (peerUser peer)
  group <- traverse (\gid -> fromMaybe (decimal gid) <$> udGroupName db gid) (peerGroup peer)
  let matches = matchUser db peer user group
  trusted <- matches (asTrustedUsers settings)
  allowed <- if trusted then pure True else matches (asAllowedUsers settings)
  pure $
    if not allowed || group == Just (asBuildUsersGroup settings)
      then Refused ("user '" <> maybe unknownName TE.decodeUtf8Lenient user <> "' is not allowed to connect to the Nix daemon")
      else Admitted (if trusted then Trusted else NotTrusted) user

-- | Whether a list of users matches the peer: @*@ matches everyone, a
-- name the peer's user's name, and @\@GROUP@ a peer whose group is
-- named so or whose user the group lists, or, where the system answers
-- membership itself, one it says is a member.
matchUser :: (Monad m) => UserDatabase m -> Peer -> Maybe ByteString -> Maybe ByteString -> [ByteString] -> m Bool
matchUser db peer user group users
  | everyone `elem` users = pure True
  | maybe False (`elem` users) user = pure True
  | otherwise = anyM groupMatches [name | entry <- users, Just name <- [BS.stripPrefix groupMark entry]]
  where
    groupMatches name
      | group == Just name = pure True
      | otherwise =
          udGroup db name >>= \case
            Nothing -> pure False
            Just entry -> maybe (pure False) (`listedIn` entry) user
    listedIn name entry
      | name `elem` groupEntryMembers entry = pure True
      | otherwise = maybe (pure False) (\uid -> fromMaybe False <$> udMembership db uid (groupEntryId entry)) (peerUser peer)

-- | Whether any of the tests passes, trying them in order until one does.
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM test = \case
  [] -> pure False
  item : rest -> test item >>= \passed -> if passed then pure True else anyM test rest

-- | The entry that matches every user, and the mark of a group.
everyone, groupMark :: ByteString
everyone = "*"
groupMark = "@"

-- | A number as upstream names a user or group without an entry.
decimal :: Word32 -> ByteString
decimal = BS8.pack . show

-- | How a peer with no user is named.
unknownName :: Text
unknownName = "<unknown>"

-- | The line upstream's daemon logs as it accepts a connection.
acceptedMessage :: Peer -> Admission -> Text
acceptedMessage peer admission =
  "accepted connection from pid "
    <> maybe unknownName (T.pack . show) (peerProcess peer)
    <> ", user "
    <> named
  where
    named = case admission of
      Admitted Trusted user -> userText user <> " (trusted)"
      Admitted NotTrusted user -> userText user
      Refused _ -> unknownName
    userText = maybe unknownName TE.decodeUtf8Lenient
