-- |
-- Module:      Data.RTCM3
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 message containers.

module Data.RTCM3
  ( RTCM3Msg (..)
  , module Data.RTCM3.Types
  , module Data.RTCM3.Observations
  ) where

import           BasicPrelude
import           Data.Binary
import qualified Data.Binary.Bits.Get as B
import           Data.ByteString.Lazy
import           Data.RTCM3.Observations
import           Data.RTCM3.Types

data RTCM3Msg =
     RTCM3Msg1001 Msg1001 Msg
   | RTCM3Msg1002 Msg1002 Msg
   | RTCM3Msg1003 Msg1003 Msg
   | RTCM3Msg1004 Msg1004 Msg
   | RTCM3MsgUnknown Word16 Msg
   | RTCM3MsgBadCrc Msg
   deriving ( Show, Read, Eq )

instance Binary RTCM3Msg where
  get = do
    preamble <- getWord8
    if preamble /= msgRTCM3Preamble then get else do
      rtcm3 <- get
      decode' rtcm3 where
        decode' rtcm3@Msg {..}
          | checkCrc rtcm3 /= _msgRTCM3Crc = return $ RTCM3MsgBadCrc rtcm3
          | otherwise = B.runBitGet $ do
              num <- B.getWord16be 12
              decode'' num where
                decode'' num
                  | num == msg1001 = return $ RTCM3Msg1001 (decode $ fromStrict _msgRTCM3Payload) rtcm3
                  | num == msg1002 = return $ RTCM3Msg1002 (decode $ fromStrict _msgRTCM3Payload) rtcm3
                  | num == msg1003 = return $ RTCM3Msg1003 (decode $ fromStrict _msgRTCM3Payload) rtcm3
                  | num == msg1004 = return $ RTCM3Msg1004 (decode $ fromStrict _msgRTCM3Payload) rtcm3
                  | otherwise = return $ RTCM3MsgUnknown num rtcm3

  put msg = do
    putWord8 msgRTCM3Preamble
    put $ encode' msg where
      encode' (RTCM3Msg1001 _msg rtcm3) = rtcm3
      encode' (RTCM3Msg1002 _msg rtcm3) = rtcm3
      encode' (RTCM3Msg1003 _msg rtcm3) = rtcm3
      encode' (RTCM3Msg1004 _msg rtcm3) = rtcm3
      encode' (RTCM3MsgUnknown _num rtcm3) = rtcm3
      encode' (RTCM3MsgBadCrc rtcm3) = rtcm3
