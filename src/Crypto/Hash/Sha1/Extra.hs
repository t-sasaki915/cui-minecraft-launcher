module Crypto.Hash.Sha1.Extra (stringHash) where

import           Crypto.Hash.SHA1 (hash)
import           Data.ByteString  (ByteString, unpack)
import           Text.Printf      (printf)

stringHash :: ByteString -> String
stringHash raw = unpack (hash raw) >>= printf "%02x"
