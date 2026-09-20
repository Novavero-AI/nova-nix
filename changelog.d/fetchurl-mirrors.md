- **Windows source fetches retry ordered mirrors and verify every download.**
  Nova's package fetcher accepts a non-empty `urls` list for flat fixed-output
  downloads. HTTP errors, incomplete responses, and hash mismatches advance
  to the next URL; only verified bytes enter the store. Cancellation stops
  the build and releases its output lock without trying another mirror.
  Hello, sed, and zlib use multiple pinned source locations. Their source
  and package output paths stay unchanged, while their derivation paths
  change to describe the new inputs. The bundled upstream `fetchurl.nix`
  remains unchanged, and single-URL calls retain its interface.
