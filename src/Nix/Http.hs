-- | The one User-Agent every HTTP request nova-nix makes carries.
--
-- Upstream Nix identifies itself on every request as @curl\/VERSION
-- Nix\/VERSION@.  A request with no User-Agent is what bot mitigation in
-- front of a server scores lowest: ftp.gnu.org answers one with 403
-- outright, and Cloudflare in front of cache.novavero.ai challenged every
-- request a GitHub runner made without one while passing the same requests
-- carrying this header.  The fetcher sent one; the substituter and push
-- sent none, so the public cache could be read by a person and not by the
-- tool built to read it.  The version comes from Cabal, so the header
-- names the build that made the request rather than a spelling that can
-- drift at release time.
module Nix.Http
  ( userAgent,
    withUserAgent,
  )
where

import qualified Data.ByteString.Char8 as BS8
import Data.Version (showVersion)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Header (hUserAgent)
import Paths_nova_nix (version)

-- | @nova-nix\/VERSION (+repository)@: the product token upstream's shape
-- calls for, and a comment naming where the client comes from.
userAgent :: BS8.ByteString
userAgent = BS8.pack ("nova-nix/" <> showVersion version <> " (+https://github.com/Novavero-AI/nova-nix)")

-- | The request with 'userAgent' as its only User-Agent, whatever it
-- carried before.
withUserAgent :: HTTP.Request -> HTTP.Request
withUserAgent request =
  request
    { HTTP.requestHeaders =
        (hUserAgent, userAgent) : filter ((/= hUserAgent) . fst) (HTTP.requestHeaders request)
    }
