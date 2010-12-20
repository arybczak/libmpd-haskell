-- | Module    : Network.MPD.Commands.Util
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Internal utilities for implementing MPD commands.

module Network.MPD.Commands.Util where

import Network.MPD.Core

import qualified Data.ByteString.Char8 as B
import Control.Monad.Error (throwError)
import Data.List (intersperse)

-- A wrapper for receive that discards empty response and fails on non-empty one.
receive_ :: MonadMPD m => m ()
receive_ =
    receive >>= \ls ->
    if null ls
       then return ()
       else throwError $ Unexpected "Empty response expected."

-- A wrapper for receive that accepts one-line responses and fails otherwise.
receive1 :: MonadMPD m => m [B.ByteString]
receive1 =
    receive >>= \ls ->
    let err_msg = "One line long response expected, got " ++ show ls_len ++ " lines."
        ls_len = length ls in
    if ls_len /= 1
       then throwError . Unexpected $ err_msg
       else return ls

-- Send many commands (using commands list)
sendMany :: MonadMPD m => [String] -> m ()
sendMany cmds = send . concat $ intersperse "\n" cmds'
    where cmds' = "command_list_begin" : cmds ++ ["command_list_end"]
