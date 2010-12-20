-- | Module    : Network.MPD.Commands
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Interface to the user commands supported by MPD.

module Network.MPD.Commands (
    -- * Command related data types
    module Network.MPD.Commands.Types,

    -- * Query interface
    module Network.MPD.Commands.Query,

    -- * Querying MPD's status
    clearError, currentSong, idle, getIdle, noidle, status, stats,

    -- * Playback options
    consume, crossfade, random, repeat, setVolume, single, replayGainMode,
    replayGainStatus,

    -- * Controlling playback
    next, pause, play, playId, previous, seek, seekId, stop,

    -- * The current playlist
    add, addId, clear, delete, deleteId, move, moveId, playlist,
    playlistFind, playlistInfo, playlistSearch, plChanges,
    plChangesPosId, shuffle, swap, swapId,

    -- * Stored playlist
    listPlaylist, listPlaylistInfo, listPlaylists, load, playlistAdd,
    playlistClear, playlistDelete, playlistMove, rename, rm, save,

    -- * The music database
    count, find, findAdd, list, listAlbums, listAll, listAllInfo, lsInfo,
    search, update, rescan,

    -- * Stickers
    stickerGet, stickerSet, stickerDelete, stickerList, stickerFind,

    -- * Connection
    close, kill, password, ping,

    -- * Audio output devices
    disableOutput, enableOutput, outputs,

    -- * Reflection
    commands, notCommands, tagTypes, urlHandlers, decoders,
    ) where

import Network.MPD.Commands.Arg
import Network.MPD.Commands.Parse
import Network.MPD.Commands.Query
import Network.MPD.Commands.Types
import Network.MPD.Commands.Util
import Network.MPD.Core
import Network.MPD.Utils

import qualified Data.ByteString.Char8 as B
import Prelude hiding (repeat)
import Control.Monad.Error (catchError, throwError)

--
-- Querying MPD's status
--

-- | Clear the current error message in status.
clearError :: MonadMPD m => m ()
clearError = send "clearerror" >> receive_

-- | Get the current song.
currentSong :: MonadMPD m => m (Maybe Song)
currentSong =
    send "currentsong" >> receive >>= songFold (const . return . Just) Nothing

-- | Make MPD server notify the client if there is a noteworthy change
-- in one or more of its subsystems. Note that after running this command
-- you can either monitor handle for incoming notifications, wait for events
-- using 'getIdle or cancel this by 'noidle'. Any command other than 'noidle'
-- sent to MPD server while idle is active will close the connection.
idle :: MonadMPD m => [Subsystem] -> m ()
idle ss = send ("idle" <$> foldr (<++>) (Args []) ss)

-- | Fold over idle notifications. If there is no notifications ready
--   at the moment, this function will block until they show up.
getIdle :: MonadMPD m => (Subsystem -> b -> m b) -> b -> m b
getIdle f acc = receive >>= subsystemFold f acc

-- | Cancel 'idle'.
noidle :: MonadMPD m => m ()
noidle = send "noidle" >> receive_

-- | Get server statistics.
stats :: MonadMPD m => m (Maybe Stats)
stats = send "stats" >> receive >>= genStats

-- | Get the server's status.
status :: MonadMPD m => m (Maybe Status)
status = send "status" >> receive >>= genStatus

--
-- Playback options
--

-- | Set consume mode
consume :: MonadMPD m => Bool -> m ()
consume v = send ("consume" <$> v) >> receive_

-- | Set crossfading between songs.
crossfade :: MonadMPD m => Int -> m ()
crossfade secs = send ("crossfade" <$> secs) >> receive_

-- | Set random playing.
random :: MonadMPD m => Bool -> m ()
random v = send ("random" <$> v) >> receive_

-- | Set repeating.
repeat :: MonadMPD m => Bool -> m ()
repeat v = send ("repeat" <$> v) >> receive_

-- | Set the volume (0-100 percent).
setVolume :: MonadMPD m => Int -> m ()
setVolume v = send ("setvol" <$> v) >> receive_

-- | Set single mode
single :: MonadMPD m => Bool -> m ()
single v = send ("single" <$> v) >> receive_

-- | Set the replay gain mode.
replayGainMode :: MonadMPD m => ReplayGainMode -> m ()
replayGainMode mode = send ("replay_gain_mode" <$> mode) >> receive_

-- | Get the replay gain options.
replayGainStatus :: MonadMPD m => m (Maybe ReplayGainMode)
replayGainStatus = do
    send "replay_gain_status"
    receive1 >>= return . genReplayGainMode . toAssoc . head

--
-- Controlling playback
--

-- | Play the next song.
next :: MonadMPD m => m ()
next = send "next" >> receive_

-- | Pause playing.
pause :: MonadMPD m => Bool -> m ()
pause v = send ("pause" <$> v) >> receive_

-- | Begin\/continue playing.
play :: MonadMPD m => Maybe Int -> m ()
play (Just pos) = send ("play" <$> pos) >> receive_
play _          = send  "play"          >> receive_

-- | Play a file with given id.
playId :: MonadMPD m => Int -> m ()
playId id' = send ("playid" <$> id') >> receive_

-- | Play the previous song.
previous :: MonadMPD m => m ()
previous = send "previous" >> receive_

-- | Seek to some point in a song.
seek :: MonadMPD m => Int -> Int -> m ()
seek pos time = send ("seek" <$> pos <++> time) >> receive_

-- | Seek to some point in a song (id version)
seekId :: MonadMPD m => Int -> Int -> m ()
seekId id' time = send ("seekid" <$> id' <++> time) >> receive_

-- | Stop playing.
stop :: MonadMPD m => m ()
stop = send "stop" >> receive_

--
-- The current playlist
--

-- | Add a song (or a whole directory) to the current playlist.
add :: MonadMPD m => String -> m ()
add path = send ("add" <$> path) >> receive_

-- | Like 'add', but returns a song id.
addId :: MonadMPD m => String -> Maybe Int -> m (Maybe Int)
addId path pos = do
    send ("addid" <$> path <++> pos)
    receive1 >>= return . parseInt . snd . toAssoc . head

-- | Clear the current playlist.
clear :: MonadMPD m => m ()
clear = send "clear" >> receive_

-- | Remove a song from the current playlist.
delete :: MonadMPD m => Int -> m ()
delete pos = send ("delete" <$> pos) >> receive_

-- | Remove a song from the current playlist.
deleteId :: MonadMPD m => Int -> m ()
deleteId id' = send ("deleteid" <$> id') >> receive_

-- | Move a song to a given position in the current playlist.
move :: MonadMPD m => Int -> Int -> m ()
move pos to = send ("move" <$> pos <++> to) >> receive_

-- | Move a song from (songid) to (playlist index) in the playlist. If to is
-- negative, it is relative to the current song in the playlist (if there is one).
moveId :: MonadMPD m => Int -> Int -> m ()
moveId id' to = send ("moveid" <$> id' <++> to) >> receive_

-- | Fold over file paths and positions of songs in the current playlist.
-- Note that this command is only included for completeness sake; it's
-- deprecated and likely to disappear at any time, please use 'playlistInfo'
-- instead.
playlist :: MonadMPD m => ((Int, B.ByteString) -> b -> m b) -> b -> m b
playlist f acc = send "playlist" >> receive >>= posPathFold f acc

-- | Fold over a list of songs in the playlist.
-- If first argument is:
--  - Just Left, only one song is returned.
--  - Just Right, songs from given range are returned
--  - Nothing, whole playlist is returned
playlistInfo :: MonadMPD m => Maybe (Either Int (Int, Int))
                -> (Song -> b -> m b) -> b -> m b
playlistInfo bounds f acc =
    send ("playlistinfo" <$> bounds) >> receive >>= songFold f acc

-- | Search for songs in the current playlist with
-- strict matching and fold over a list of results.
playlistFind :: MonadMPD m => Query -> (Song -> b -> m b) -> b -> m b
playlistFind q f acc = do
    send ("playlistfind" <$> q)
    receive >>= songFold f acc

-- | Search case-insensitively with partial matches for
-- songs in the current playlist and fold over a list results.
playlistSearch :: MonadMPD m => Query -> (Song -> b -> m b) -> b -> m b
playlistSearch q f acc =
    send ("playlistsearch" <$> q) >> receive >>= songFold f acc

-- | Fold over a list of changed songs currently in the playlist since
-- a given playlist version.
plChanges :: MonadMPD m => Integer -> (Song -> b -> m b) -> b -> m b
plChanges version f acc = do
    send ("plchanges" <$> version)
    receive >>= songFold f acc

-- | Like 'playlistChanges' but only returns positions and ids.
plChangesPosId :: MonadMPD m => Integer -> ((Int, Int) -> b -> m b) -> b -> m b
plChangesPosId version f acc =
    send ("plchangesposid" <$> version) >> receive >>= cposIdFold f acc

-- | Shuffle the playlist or the part of it
shuffle :: MonadMPD m => Maybe (Int, Int) -> m ()
shuffle range = send ("shuffle" <$> range) >> receive_

-- | Swap the positions of two songs with given pos.
swap :: MonadMPD m => Int -> Int -> m ()
swap pos1 pos2 = send ("swap" <$> pos1 <++> pos2) >> receive_

-- | Swap the positions of two songs with given ids.
swapId :: MonadMPD m => Int -> Int -> m ()
swapId id1 id2 = send ("swapid" <$> id1 <++> id2) >> receive_

--
-- Stored playlists
--

-- | Retrieve a list of files in a given playlist.
listPlaylist :: MonadMPD m => String -> (B.ByteString -> b -> m b) -> b -> m b
listPlaylist name f acc =
    send ("listplaylist" <$> name) >> receive >>= elementNamedFold "file" f acc

   -- | Fold over a list of songs in a given playlist.
listPlaylistInfo :: MonadMPD m => String -> (Song -> b -> m b) -> b -> m b
listPlaylistInfo name f acc =
    send ("listplaylistinfo" <$> name) >> receive >>= songFold f acc

-- | Retreive a list of stored playlists.
listPlaylists :: MonadMPD m => (Playlist -> b -> m b) -> b -> m b
listPlaylists f acc =
    send "listplaylists" >>  receive >>= playlistFold f acc

-- | Load an existing playlist.
load :: MonadMPD m => String -> m ()
load name = send ("load" <$> name) >> receive_

-- | Add a song (or a whole directory) to a stored playlist.
-- Will create a new playlist if the one specified does not already exist.
playlistAdd :: MonadMPD m => String -> String -> m ()
playlistAdd name path =
    send ("playlistadd" <$> name <++> path) >> receive_

-- | Clear a playlist. If the specified playlist does not exist, it will be
-- created.
playlistClear :: MonadMPD m => String -> m ()
playlistClear name = send ("playlistclear" <$> name) >> receive_


-- | Remove a song from a playlist.
playlistDelete :: MonadMPD m => String
               -> Int -- ^ Playlist position
               -> m ()
playlistDelete name pos =
    send ("playlistdelete" <$> name <++> pos) >> receive_

-- | Move a song to a given position in the playlist specified.
playlistMove :: MonadMPD m => String -> Int -> Int -> m ()
playlistMove name from to =
    send ("playlistmove" <$> name <++> from <++> to) >> receive_

-- | Rename an existing playlist.
rename :: MonadMPD m
       => String -- ^ Original playlist
       -> String -- ^ New playlist name
       -> m ()
rename old new = send ("rename" <$> old <++> new) >> receive_

-- | Delete existing playlist.
rm :: MonadMPD m => String -> m ()
rm name = send ("rm" <$> name) >> receive_

-- | Save the current playlist.
save :: MonadMPD m => String -> m ()
save name = send ("save" <$> name) >> receive_

--
-- The music database
--

-- | Count the number of entries matching a query.
count :: MonadMPD m => Query -> m (Maybe Count)
count query =
    send ("count" <$> query) >> receive >>= genCount

-- | Search the database for entries exactly matching
-- a query and fold over a list of results.
find :: MonadMPD m => Query -> (Song -> b -> m b) -> b -> m b
find query f acc =
    send ("find" <$> query) >> receive >>= songFold f acc

-- | Adds songs matching a query to the current playlist.
findAdd :: MonadMPD m => Query -> m ()
findAdd query = send ("findadd" <$> query) >> receive_

-- | Fold over a list of all tags of the specified type.
list :: MonadMPD m => Metadata -> (B.ByteString -> b -> m b) -> b -> m b
list meta f acc =
    send ("list" <$> meta) >> receive >>= elementNamedFold (show meta) f acc

-- | Fold over a list of albums of given artist.
listAlbums :: MonadMPD m => String -> (B.ByteString -> b -> m b) -> b -> m b
listAlbums artist f acc = do
    send ("list" <$> Album <++> artist)
    receive >>= elementNamedFold "Album" f acc

-- | Fold over a list of songs (without metadata) in a database directory recursively.
listAll :: MonadMPD m => String -> (B.ByteString -> b -> m b) -> b -> m b
listAll path f acc =
    send ("listall" <$> path) >> receive >>= elementNamedFold "file" f acc

-- | Non-recursively fold over the content of a database directory.
lsInfo :: MonadMPD m => String -> (Entry -> b -> m b) -> b -> m b
lsInfo path f acc =
    send ("lsinfo" <$> path) >> receive >>= entryFold f acc

-- | Recursive 'lsInfo'.
listAllInfo :: MonadMPD m => String -> (Entry -> b -> m b) -> b -> m b
listAllInfo path f acc =
    send ("listallinfo" <$> path) >> receive >>= entryFold f acc

-- | Search the database using case insensitive matching and fold over a list of results.
search :: MonadMPD m => Query -> (Song -> b -> m b) -> b -> m b
search query f acc = send ("search" <$> query) >> receive >>= songFold f acc

-- | Update the server's database and fold over a list of job ids.
-- If no paths are given, all database will be scanned.
-- Unreadable or non-existent paths are silently ignored.
update :: MonadMPD m => [String] -> (Int -> b -> m b) -> b -> m b
update xs f acc = do
    case xs of
         []  -> send "update"
         [x] -> send ("update" <$> x)
         _   -> sendMany (map ("update" <$>) xs)
    receive >>= jobIdFold f acc

-- | Like 'update' but also rescans unmodified files.
rescan :: MonadMPD m => [String] -> (Int -> b -> m b) -> b -> m b
rescan xs f acc = do
    case xs of
         []  -> send "rescan"
         [x] -> send ("rescan" <$> x)
         _   -> sendMany (map ("rescan" <$>) xs)
    receive >>= jobIdFold f acc

--
-- Stickers
--

-- | Reads a sticker value for the specified object.
-- Note: if sticker is not present, it returns Nothning
-- instead of throwing exception.
stickerGet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> m (Maybe Sticker)
stickerGet otype uri name = do
    send ("sticker get" <$> otype <++> uri <++> name)
    receive1 `catchError` check >>= stickerFold (const . return . Just) Nothing
    where
          check (ACK FileNotFound "sticker" "no such sticker") = return []
          check e = throwError e

-- | Adds a sticker value to the specified object.
stickerSet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> String -- ^ Sticker value
           -> m ()
stickerSet otype uri name value =
    send ("sticker set" <$> otype <++> uri <++> name <++> value) >> receive_

-- | Delete a sticker value from the specified object.
stickerDelete :: MonadMPD m => ObjectType
              -> String -- ^ Object URI
              -> String -- ^ Sticker name
              -> m ()
stickerDelete otype uri name =
    send ("sticker delete" <$> otype <++> uri <++> name) >> receive_

-- | Fold over a stickers list of the specified object.
stickerList :: MonadMPD m => ObjectType
            -> String -- ^ Object URI
            -> (Sticker -> b -> m b) -> b -> m b
stickerList otype uri f acc =
    send ("sticker list" <$> otype <++> uri) >> receive >>= stickerFold f acc

-- | Searches the sticker database for stickers with the specified name, below
-- the specified path and folds over a list of results.
stickerFind :: MonadMPD m => ObjectType
            -> String -- ^ Path
            -> String -- ^ Sticker name
            -> ((B.ByteString, Sticker) -> b -> m b) -> b -> m b
stickerFind otype uri name f acc = do
    send ("sticker find" <$> otype <++> uri <++> name)
    receive >>= ownedStickerFold f acc

--
-- Connection
--

-- | Send password to server to authenticate session.
-- Password is sent as plain text.
-- Note: password has to be set previously using
-- setPassword function (or specified in host variable)
password :: MonadMPD m => m ()
password =
    getPassword >>= send . ("password " <$>) >> receive_

-- | Check that the server is still responding.
ping :: MonadMPD m => m ()
ping = send "ping" >> receive_

--
-- Audio output devices
--

-- | Turn off an output device.
disableOutput :: MonadMPD m => Int -> m ()
disableOutput oid = send ("disableoutput" <$> oid) >> receive_

-- | Turn on an output device.
enableOutput :: MonadMPD m => Int -> m ()
enableOutput oid = send ("enableoutput" <$> oid) >> receive_

-- | Fold over a list of output devices.
outputs :: MonadMPD m => (Output -> b -> m b) -> b -> m b
outputs f acc =
    send "outputs" >> receive >>= outputFold f acc

--
-- Reflection
--

-- | Fold over a list of available commands.
commands :: MonadMPD m => (B.ByteString -> b -> m b) -> b -> m b
commands f acc =
    send "commands" >> receive >>= elementNamedFold "command" f acc

-- | Fold over a list of unavailable (due to access restrictions) commands.
notCommands :: MonadMPD m => (B.ByteString -> b -> m b) -> b -> m b
notCommands f acc =
    send "notcommands" >> receive >>= elementNamedFold "command" f acc

-- | Retrieve a list of available song metadata.
tagTypes :: MonadMPD m => (B.ByteString -> b -> m b) -> b -> m b
tagTypes f acc =
    send "tagtypes" >> receive >>= elementNamedFold "tagtype" f acc

-- | Fold over a list of supported urlhandlers.
urlHandlers :: MonadMPD m => (B.ByteString -> b -> m b) -> b -> m b
urlHandlers f acc=
    send "urlhandlers" >> receive >>= elementNamedFold "handler" f acc

-- | Fold over a list of decoder plugins with theirs suffixes and mime types.
decoders :: MonadMPD m => (Decoder -> b -> m b) -> b -> m b
decoders f acc =
    send "decoders" >> receive >>= decoderFold f acc
