-- | Module    : Network.MPD.Core.Error
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- MPD errors.

module Network.MPD.Core.Error where

import Control.Monad.Error (Error(..))

-- | The MPDError type is used to signal errors, both from the MPD and
-- otherwise.
data MPDError = NoMPD              -- ^ No active connection to MPD
              | ConnError String   -- ^ Error while connecting to MPD
              | ConnLost String    -- ^ Current connection is no longer valid
              | TimedOut           -- ^ The connection timed out
              | Unexpected String  -- ^ MPD returned an unexpected response.
                                   --   This is a bug, either in the library or
                                   --   in MPD itself.
              | Custom String      -- ^ Used for misc. errors
              | ACK {
                  ackType :: ACKType -- ^ ACK type
                , ackCmd  :: String  -- ^ command that failed
                , ackMsg  :: String  -- ^ message from the server
                } deriving Eq

instance Show MPDError where
    show NoMPD          = "No active connection"
    show (ConnError s)  = "Could not connect to MPD: " ++ s
    show (ConnLost s)   = "Connection to MPD lost: " ++ s
    show TimedOut       = "MPD connection timed out"
    show (Unexpected s) = "MPD returned an unexpected response: " ++ s
    show (Custom s)     = s
    show ack            = ackCmd ack ++ ": " ++ ackMsg ack

instance Error MPDError where
    noMsg  = Custom "An error occurred"
    strMsg = Custom

-- | Represents various MPD errors (aka. ACKs).
data ACKType = InvalidArgument  -- ^ Invalid argument passed (ACK 2)
             | InvalidPassword  -- ^ Invalid password supplied (ACK 3)
             | Auth             -- ^ Authentication required (ACK 4)
             | UnknownCommand   -- ^ Unknown command (ACK 5)
             | FileNotFound     -- ^ File or directory not found ACK 50)
             | PlaylistMax      -- ^ Playlist at maximum size (ACK 51)
             | System           -- ^ A system error (ACK 52)
             | PlaylistLoad     -- ^ Playlist loading failed (ACK 53)
             | Busy             -- ^ Update already running (ACK 54)
             | NotPlaying       -- ^ An operation requiring playback
                                --   got interrupted (ACK 55)
             | FileExists       -- ^ File already exists (ACK 56)
             | UnknownACK       -- ^ An unknown ACK (aka. bug)
               deriving Eq
