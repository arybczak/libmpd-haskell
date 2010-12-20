{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

-- | Module    : Network.MPD.Core
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- The core datatypes and operations are defined here, including the
-- primary instance of the 'MonadMPD' class, 'MPD'.

module Network.MPD.Core (
    -- * Classes
    MonadMPD(..),
    -- * Data types
    MPD(..), MPDError(..), ACKType(..), Response,
    -- * Running
    withMPDEx,
    -- * Interacting
    kill, parseAck, stHandle
    ) where

import Network.MPD.Utils
import Network.MPD.Core.Class
import Network.MPD.Core.Error

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative(..))
import Control.Exception (try)
import Control.Monad (ap, unless)
import Control.Monad.Error (ErrorT(..), MonadError(..))
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.State (StateT, MonadIO(..), modify, get, evalStateT)
import qualified Data.Foldable as F
import Network (PortID(..), withSocketsDo, connectTo)
import System.IO (Handle, hPutStrLn, hClose, hFlush)
import System.IO.Unsafe (unsafeInterleaveIO)

--
-- IO based MPD client implementation.
--

-- | The main implementation of an MPD client.  It actually connects
--   to a server and interacts with it.
--
-- To use the error throwing\/catching capabilities:
--
-- > import Control.Monad.Error (throwError, catchError)
--
-- To run IO actions within the MPD monad:
--
-- > import Control.Monad.Trans (liftIO)

newtype MPD a =
    MPD { runMPD :: ErrorT MPDError
                    (StateT MPDState
                     (ReaderT (String, Int) IO)) a
        } deriving (Functor, Monad, MonadIO, MonadError MPDError)

instance Applicative MPD where
    (<*>) = ap
    pure  = return

instance MonadMPD MPD where
    open  = mpdOpen
    close = mpdClose
    send  = mpdSend
    receive = mpdReceive
    getHandle = MPD $ get >>= return . stHandle
    getPassword = MPD $ get >>= return . stPassword
    setPassword pw = MPD $ modify (\st -> st { stPassword = pw })
    getVersion = MPD $ get >>= return . stVersion

-- | Inner state for MPD
data MPDState =
    MPDState { stHandle   :: Maybe Handle
             , stPassword :: String
             , stVersion  :: (Int, Int, Int)
             }

-- | A response is either an 'MPDError' or some result.
type Response = Either MPDError

-- | The most configurable API for running an MPD action.
withMPDEx :: String -> Int -> String -> MPD a -> IO (Response a)
withMPDEx host port pw x = withSocketsDo $
    runReaderT (evalStateT (runErrorT . runMPD $ open >> x) initState)
               (host, port)
    where
          initState = MPDState Nothing pw (0, 0, 0)

mpdOpen :: MPD ()
mpdOpen = MPD $ do
    (host, port) <- ask
    runMPD close
    tryIO (connectToMPD host port)
          (throwError . ConnError . show)
          (\handle -> modify (\st -> st { stHandle = Just handle }))
    runMPD checkConn >>= flip unless
        (runMPD close >> throwError (ConnError "Validation failed"))
    where
        connectToMPD host@('/':_) _ =
            connectTo "" (UnixSocket host)
        connectToMPD host port =
            connectTo host (PortNumber $ fromIntegral port)

        checkConn = do
            msg <- mpdReceive >>= checkMsg
            if B.pack "OK MPD" `B.isPrefixOf` msg
               then do
                   MPD $ maybe (throwError $ Custom "Couldn't determine MPD version")
                               (\v -> modify (\st -> st { stVersion = v }))
                               (parseVersion msg)
                   return True
               else return False

        checkMsg ls =
            if null ls
               then throwError $ Custom "No welcome message"
               else return $ head ls

        parseVersion = parseTriple '.' parseInt . B.dropWhile (not . isDigit)

mpdClose :: MPD ()
mpdClose = MPD $
    get >>= F.mapM_ (\handle -> do
        liftIO $ hClose handle
        modify (\st -> st { stHandle = Nothing })
    ) . stHandle

mpdSend :: String -> MPD ()
mpdSend str = MPD $
    get >>= maybe (throwError NoMPD) (\handle ->
        if null str
           then return ()
           else tryIO (hPutStrLn handle str >> hFlush handle)
                      (\e -> runMPD close >> throwError (ConnLost $ show e))
                      return
    ) . stHandle

mpdReceive :: MPD [B.ByteString]
mpdReceive = MPD $
    get >>= maybe (throwError NoMPD) (\handle ->
        tryIO (getLines handle)
              (\e -> runMPD close >> throwError (ConnLost $ show e))
              (\ls -> do
                  let l = head ls
                  if ack `B.isPrefixOf` l
                     then throwError . parseAck $ B.unpack l
                     else return $ takeWhile (/= ok) ls)
    ) . stHandle
    where
          getLines h = do
              l <- B.hGetLine h
              if ack `B.isPrefixOf` l || ok `B.isPrefixOf` l
                 then return [l]
                 else unsafeInterleaveIO (getLines h) >>= return . (l:)
          
          ack = B.pack "ACK"
          ok  = B.pack "OK"

-- | Executes IO action and runs appropriate function depending on its result.
tryIO :: (MonadIO m) => IO a -> (IOError -> m b) -> (a -> m b) -> m b
tryIO f err ok = (liftIO . try $ f) >>= either err ok

--
-- Other operations.
--

-- | Kill the server. Obviously, the connection is then invalid.
kill :: (MonadMPD m) => m ()
kill = send "kill" >> close

-- Turn MPD ACK into the corresponding 'MPDError'
parseAck :: String -> MPDError
parseAck s = ACK ack cmd msg
    where
        ack = case code of
                2  -> InvalidArgument
                3  -> InvalidPassword
                4  -> Auth
                5  -> UnknownCommand
                50 -> FileNotFound
                51 -> PlaylistMax
                52 -> System
                53 -> PlaylistLoad
                54 -> Busy
                55 -> NotPlaying
                56 -> FileExists
                _  -> UnknownACK
        (code, cmd, msg) = splitAck s

-- Break an ACK into (error code, current command, message).
-- ACKs are of the form:
-- ACK [error@command_listNum] {current_command} message_text\n
splitAck :: String -> (Int, String, String)
splitAck s = (fromMaybe (-1) $ parseNum code, cmd, msg)
    where
        (code, notCode) = between '[' '@' s
        (cmd, notCmd)   = between '{' '}' notCode
        msg             = dropWhile (' ' ==) . drop 1 $ notCmd
        
        between a b     = break (== b) . drop 1 . dropWhile (/= a)
