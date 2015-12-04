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

msg1002 :: Word16
msg1002 = 1002

data Msg1002 = Msg1002
  {
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1002)

instance Binary Msg1002 where
  get = return Msg1002

  put _ = return ()

$(deriveRTCM3 'msg1002 ''Msg1002)

msg1003 :: Word16
msg1003 = 1003

data Msg1003 = Msg1003
  {
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1003)

instance Binary Msg1003 where
  get = return Msg1003

  put _ = return ()

$(deriveRTCM3 'msg1003 ''Msg1003)

msg1004 :: Word16
msg1004 = 1004

data Msg1004 = Msg1004
  {
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1004)

instance Binary Msg1004 where
  get = return Msg1004

  put _ = return ()

$(deriveRTCM3 'msg1004 ''Msg1004)
