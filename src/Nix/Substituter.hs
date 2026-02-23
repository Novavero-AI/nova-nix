-- | Binary substituter — download pre-built paths from remote caches.
--
-- == How substitution works
--
-- Before building a derivation, Nix checks if the output already exists
-- in a binary cache.  The protocol:
--
-- 1. Compute the output store path hash from the derivation
-- 2. @GET https:\/\/cache.example.com\/\<hash\>.narinfo@
-- 3. If 200: parse the narinfo (NAR hash, size, references, signature)
-- 4. Verify the signature against a trusted public key
-- 5. @GET https:\/\/cache.example.com\/nar\/\<narhash\>.nar.xz@
-- 6. Decompress, verify NAR hash, unpack into store path
-- 7. Register in the store DB with references from narinfo
--
-- If the cache doesn't have it (404), fall through to building locally.
--
-- == Cache priority
--
-- Multiple caches can be configured, checked in priority order:
--
-- @
-- substituters = https:\/\/cache.novavero.ai https:\/\/cache.nixos.org
-- trusted-public-keys = cache.novavero.ai-1:... cache.nixos.org-1:...
-- @
--
-- Our nova-cache server implements this protocol.  The narinfo format,
-- NAR serialization, signature verification — all handled by the
-- @nova-cache@ library.  This module just orchestrates the HTTP requests
-- and store registration.
module Nix.Substituter
  ( -- * Substitution
    SubstResult (..),
    trySubstitute,

    -- * Cache configuration
    CacheConfig (..),
  )
where

import Data.Text (Text)
import Nix.Store.Path (StorePath)

-- | Configuration for a binary cache.
data CacheConfig = CacheConfig
  { -- | Base URL of the cache (e.g. @https:\/\/cache.novavero.ai@).
    ccUrl :: !Text,
    -- | Trusted public key for signature verification.
    ccPublicKey :: !Text,
    -- | Priority (lower = checked first). cache.nixos.org is 40.
    ccPriority :: !Int
  }
  deriving (Eq, Show)

-- | Result of a substitution attempt.
data SubstResult
  = -- | Successfully substituted — path is now in the store.
    SubstSuccess !StorePath
  | -- | Cache doesn't have this path.
    SubstNotFound
  | -- | Download or verification failed.
    SubstError !Text
  deriving (Eq, Show)

-- | Try to substitute a store path from configured caches.
--
-- Checks each cache in priority order.  Returns on first success.
-- Uses nova-cache library for narinfo parsing, NAR unpacking,
-- and signature verification.
trySubstitute :: [CacheConfig] -> StorePath -> IO SubstResult
trySubstitute _caches _path =
  -- TODO: implement HTTP fetching + nova-cache integration
  pure SubstNotFound
