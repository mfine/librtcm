-- |
-- Module:      Data.RTCM3.Observations
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 Observations.

module Data.RTCM3.Observations where

import BasicPrelude
import Control.Lens
import Data.Binary
import Data.RTCM3.TH

msg1001 :: Word16
msg1001 = 1001

data Msg1001 = Msg1001
  {
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1001)

instance Binary Msg1001 where
  get = return Msg1001

  put _ = return ()

$(deriveRTCM3 'msg1001 ''Msg1001)
