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
  ) where

import BasicPrelude
import Data.Binary
import Data.RTCM3.Observations
import Data.RTCM3.Types

data RTCM3Msg =
     RTCM3Msg1001 Msg1001 Msg
   | RTCM3MsgBadCrc Msg
   | RTCM3MsgUnknown Msg
   deriving ( Show, Read, Eq )

instance Binary RTCM3Msg where
  get = do
    preamble <- getWord8
    if preamble /= msgRTCM3Preamble then get else do
      rtcm3 <- get
      return $ decode' rtcm3 where
        decode' rtcm3@Msg {..}
          | checkCrc rtcm3 /= _msgRTCM3Crc = RTCM3MsgBadCrc rtcm3
          | otherwise = RTCM3MsgUnknown rtcm3

  put msg = do
    putWord8 msgRTCM3Preamble
    put $ encode' msg where
      encode' (RTCM3Msg1001 _ rtcm3) = rtcm3
      encode' (RTCM3MsgBadCrc rtcm3) = rtcm3
      encode' (RTCM3MsgUnknown rtcm3) = rtcm3
