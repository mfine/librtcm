-- |
-- Module:      Data.RTCM3.TH
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Templated generation of RTCM3 interfaces.

module Data.RTCM3.TH where

import           BasicPrelude hiding ( length )
import           Control.Lens
import           Data.Binary
import           Data.Binary.Put
import qualified Data.Binary.Bits.Put as B
import           Data.ByteString.Lazy
import           Data.RTCM3.Types
import           Language.Haskell.TH

-- | Derive ToRTCM3 typeclass, given an RTCM3 message number name and the
-- name of the implemented type.
deriveRTCM3 :: Name -> Name -> Q [Dec]
deriveRTCM3 number name =
  [d|instance ToRTCM3 $(conT name) where
       toRTCM3 msg = encoded & msgRTCM3Crc .~ checkCrc encoded where
         payload = runPut $ B.runBitPut $ do
           B.putWord16be 10 $(varE number)
           B.putByteString $ toStrict $ encode msg
         encoded = Msg (fromIntegral $ length payload) (toStrict payload) 0
    |]
