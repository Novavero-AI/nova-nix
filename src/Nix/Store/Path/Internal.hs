-- | The bare 'StorePath' representation.  Importing this module is an
-- assertion: every value constructed here satisfies the store-path
-- invariants (32 nix-base32 hash characters; a name
-- 'Nix.Store.Path.checkStorePathName' accepts) - or is
-- 'maskedOutputPath', which exists only for derivation-hash masking
-- and is never a real identity.  Production code imports this at
-- exactly the validated construction gates
-- ("Nix.Store.Path", "Nix.Hash") and the two documented
-- provenance-safe sites (the C string-context unmarshal, the masking
-- placeholder).  Everything else constructs through
-- 'Nix.Store.Path.parseStorePath' \/
-- 'Nix.Store.Path.parseStorePathBaseName' and reads through the
-- exported selectors.  Tests may construct fixtures freely.
module Nix.Store.Path.Internal
  ( StorePath (..),
    maskedOutputPath,
  )
where

import Data.Text (Text)

-- | A parsed store path: the hash and name components.
data StorePath = StorePath
  { -- | The 32-character Nix base-32 hash.
    spHash :: !Text,
    -- | The human-readable name (e.g. @hello-2.12.1@).
    spName :: !Text
  }
  deriving (Eq, Ord, Show)

-- | The empty output-path placeholder that derivation-hash masking
-- renders: upstream computes a derivation's modulo hash over an ATerm
-- whose output path fields are empty strings, and this value exists
-- only to carry that rendering through 'DerivationOutput'.  It is
-- never a real store identity and must never reach the store, the
-- builder, or a context element.
maskedOutputPath :: StorePath
maskedOutputPath = StorePath "" ""
