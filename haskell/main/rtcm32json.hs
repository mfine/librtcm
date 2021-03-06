{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Main
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCM3 to JSON tool - reads RTCM3 binary from stdin and sends RTCM3 JSON
-- to stdout.

import BasicPrelude                      hiding (map)
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson.Encode
import Data.ByteString.Builder
import Data.ByteString.Lazy              hiding (ByteString, map)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import Data.Conduit.Serialization.Binary
import Data.RTCM3
import System.IO

-- | Encode a RTCM3Msg to a line of JSON.
encodeLine :: RTCM3Msg -> ByteString
encodeLine v = toStrict $ toLazyByteString $ encodeToBuilder (toJSON v) <> "\n"

main :: IO ()
main =
  runResourceT $
    sourceHandle stdin  =$=
      conduitDecode     =$=
      map encodeLine $$
      sinkHandle stdout
