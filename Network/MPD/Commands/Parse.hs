{-# LANGUAGE Rank2Types #-}

-- | Module    : Network.MPD.Commands.Parse
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Parsers for MPD data types.

module Network.MPD.Commands.Parse (
    -- * Fold generator
    parseResponse,
    -- * Object folds
    outputFold, subsystemFold, entryFold, directoryFold,
    songFold, playlistFold, posPathFold, cposIdFold, decoderFold,
    jobIdFold, stickerFold, ownedStickerFold, elementNamedFold,
    -- * Object generators
    genCount, genStats, genStatus,
    -- * Misc parsers
    genReplayGainMode
    ) where

import Network.MPD.Commands.Types

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Arrow (first, (***))
import Control.Monad (liftM)
import Network.MPD.Utils

data Iterate = Continue | Stop

type Parser a = Maybe a -> (String, B.ByteString) -> (Maybe a, Iterate)
type ObjFold a = (Monad m) => (a -> b -> m b) -> b -> [B.ByteString] -> m b
type ObjGen a = (Monad m) => [B.ByteString] -> m (Maybe a)

parseResponse :: Parser a -> ObjFold a
parseResponse parser f initial_acc ls = go Nothing ls initial_acc
    where
          go _   []         acc = return acc
          go obj ls@(l:ls') acc =
              case iter_state of
                   Stop     -> accumulate_obj False
                   Continue -> if null ls'
                                  then accumulate_obj True
                                  else go obj' ls' acc
              where
                    (obj', iter_state) = parser obj $ toAssoc l
                    accumulate_obj skip =
                        case obj' of
                             Just x  -> f x acc >>= go Nothing (if skip
                                                                   then ls'
                                                                   else ls)
                             Nothing -> go Nothing ls' acc

-------------------------------------------------------------------

-- | Generate Output object
genOutput :: Parser Output
genOutput o@(Just o') (k, v) =
    case k of
         "outputname"    -> (Just $ o' { outName = v }, Continue)
         "outputenabled" -> (Just $ o' { outEnabled = bool v }, Continue)
         "outputid"      -> (o, Stop)
         _               -> (o, Continue)

genOutput Nothing (k, v) =
    case k of
         "outputid" -> (Just . initOutput . int $ v, Continue)
         _          -> (Nothing, Continue)

initOutput id_ =
    Output { outID = id_
           , outName = B.empty
           , outEnabled = False
           }

outputFold :: ObjFold Output
outputFold = parseResponse genOutput

-------------------------------------------------------------------

-- | Generate Subsystem object
genSubsystem :: Parser Subsystem
genSubsystem Nothing (k, v) =
    case k of
         "changed" -> case B.unpack v of
                           "database"        -> (Just DatabaseS, Continue)
                           "update"          -> (Just UpdateS, Continue)
                           "stored_playlist" -> (Just StoredPlaylistS, Continue)
                           "playlist"        -> (Just PlaylistS, Continue)
                           "player"          -> (Just PlayerS, Continue)
                           "mixer"           -> (Just MixerS, Continue)
                           "output"          -> (Just OutputS, Continue)
                           "options"         -> (Just OptionsS, Continue)
                           _                 -> (Nothing, Continue)
         _         -> (Nothing, Continue)

genSubsystem s _ = (s, Stop)

subsystemFold :: ObjFold Subsystem
subsystemFold = parseResponse genSubsystem

-------------------------------------------------------------------

-- | Generate Entry object
genEntry :: Parser Entry
genEntry Nothing pair@(k, _) =
    case k of
         "directory" -> first (liftM DirectoryE) $ genDirectory Nothing pair
         "file"      -> first (liftM SongE) $ genSong Nothing pair
         "playlist"  -> first (liftM PlaylistE) $ genPlaylist Nothing pair
         _           -> (Nothing, Continue)

genEntry (Just (DirectoryE d)) pair = first (liftM DirectoryE) $ genDirectory (Just d) pair
genEntry (Just (SongE s))      pair = first (liftM SongE) $ genSong (Just s) pair
genEntry (Just (PlaylistE pl)) pair = first (liftM PlaylistE) $ genPlaylist (Just pl) pair

entryFold :: ObjFold Entry
entryFold = parseResponse genEntry

-------------------------------------------------------------------

-- | Generate Directory object
genDirectory :: Parser Directory
genDirectory Nothing (k, v) =
    case k of
         "directory" -> (Just . Directory $ v, Continue)
         _           -> (Nothing, Continue)

genDirectory d _ = (d, Stop)

directoryFold :: ObjFold Directory
directoryFold = parseResponse genDirectory

--------------------------------------------------------------------

-- | Generate Song object
genSong :: Parser Song
genSong Nothing (k, v) =
    case k of
         "file" -> (Just . initSong $ v, Continue)
         _      -> (Nothing, Continue)

genSong s@(Just s') (k, v) =
    case k of
        "Last-Modified" -> (Just $ s' { sgLastModified = parseIso8601 v }, Continue)
        "Time"          -> (Just $ s' { sgLength = int v }, Continue)
        "Id"            -> (Just $ s' { sgIndex = idval }, Continue)
        "Pos"           -> (Just $ s' { sgIndex = posval }, Continue)
        "file"          -> (s, Stop)
        "directory"     -> (s, Stop)
        "playlist"      -> (s, Stop)
        _ -> case tagValue k of
                  Just meta -> (Just $ s' { sgTags = M.insertWith' (++)
                                             meta [v] (sgTags s')
                                          }, Continue)
                  Nothing   -> (s, Continue)
    where
          idval  = Just (maybe (-1) fst $ sgIndex s', int v)
          posval = Just (int v, maybe (-1) snd $ sgIndex s')
          
          -- Why not just derive instance of Read? Because
          -- using read is a few times slower than this.
          tagValue "Artist" = Just Artist
          tagValue "ArtistSort" = Just ArtistSort
          tagValue "Album" = Just Album
          tagValue "AlbumArtist" = Just AlbumArtist
          tagValue "AlbumArtistSort" = Just AlbumArtistSort
          tagValue "Title" = Just Title
          tagValue "Track" = Just Track
          tagValue "Name" = Just Name
          tagValue "Genre" = Just Genre
          tagValue "Date" = Just Date
          tagValue "Composer" = Just Composer
          tagValue "Performer" = Just Performer
          tagValue "Disc" = Just Disc
          tagValue "MUSICBRAINZ_ARTISTID" = Just MUSICBRAINZ_ARTISTID
          tagValue "MUSICBRAINZ_ALBUMID" = Just MUSICBRAINZ_ALBUMID
          tagValue "MUSICBRAINZ_ALBUMARTISTID" = Just MUSICBRAINZ_ALBUMARTISTID
          tagValue "MUSICBRAINZ_TRACKID" = Just MUSICBRAINZ_TRACKID
          tagValue _ = Nothing

initSong path =
    Song { sgFilePath = path
         , sgTags = M.empty
         , sgLastModified = Nothing
         , sgLength = -1
         , sgIndex = Nothing }

songFold :: ObjFold Song
songFold = parseResponse genSong

-------------------------------------------------------------------

-- | Generate Playlist object
genPlaylist :: Parser Playlist
genPlaylist Nothing (k, v) =
    case k of
         "playlist" -> (Just . initPlaylist $ v, Continue)
         _          -> (Nothing, Continue)

genPlaylist pl@(Just pl') (k, v) =
    case k of
         "Last-Modified" -> (Just $ pl' { plLastModified = parseIso8601 v }, Continue)
         "playlist"      -> (pl, Stop)
         "file"          -> (pl, Stop)
         "directory"     -> (pl, Stop)
         _               -> (pl, Continue)

initPlaylist name =
    Playlist { plName = name
             , plLastModified = Nothing
             }

playlistFold :: ObjFold Playlist
playlistFold = parseResponse genPlaylist

-------------------------------------------------------------------

-- | Generate (pos, id) pair
genCposId :: Parser (Int, Int)
genCposId Nothing (k, v) =
    case k of
         "cpos" -> (Just (int v, -1), Continue)
         _      -> (Nothing, Continue)

genCposId p@(Just p') (k, v) =
    case k of
         "Id"   -> (Just (fst p', int v), Continue)
         "cpos" -> (p, Stop)
         _      -> (p, Continue)

cposIdFold :: ObjFold (Int, Int)
cposIdFold = parseResponse genCposId

-------------------------------------------------------------------

-- | Generate (pos, path) pair
genPosPath :: Parser (Int, B.ByteString)
genPosPath Nothing (k, v) =
    (Just (fromMaybe (-1) $ parseNum k, snd . toAssoc $ v), Continue)

genPosPath pp _ = (pp, Stop)

posPathFold :: ObjFold (Int, B.ByteString)
posPathFold = parseResponse genPosPath

-------------------------------------------------------------------

-- | Generate Decoder object
genDecoder :: Parser Decoder
genDecoder Nothing (k, v) =
    case k of
         "plugin"    -> (Just $ Plugin v, Continue)
         "suffix"    -> (Just $ Suffix v, Continue)
         "mime_type" -> (Just $ MimeType v, Continue)
         _           -> (Nothing, Continue)

genDecoder d _ = (d, Stop)

decoderFold :: ObjFold Decoder
decoderFold = parseResponse genDecoder

-------------------------------------------------------------------

-- | Generate JobId Object
genJobId :: Parser Int
genJobId Nothing (k, v) =
    if k == "updating_db"
       then (parseInt v, Continue)
       else (Nothing, Continue)

genJobId jid _ = (jid, Stop)

jobIdFold :: ObjFold Int
jobIdFold = parseResponse genJobId

-------------------------------------------------------------------

-- | Generate Sticker Object
genSticker :: Parser Sticker
genSticker Nothing (k, v) =
    if k == "sticker"
       then (Just $ Sticker name value, Continue)
       else (Nothing, Continue)
    where
          (name, value) = breakChar '=' v

genSticker st _ = (st, Stop)

stickerFold :: ObjFold Sticker
stickerFold = parseResponse genSticker

-------------------------------------------------------------------

-- | Generate (path, sticker) pair
genOwnedSticker :: Parser (B.ByteString, Sticker)
genOwnedSticker Nothing (k, v) =
    if k == "file"
       then (Just (v, Sticker B.empty B.empty), Continue)
       else (Nothing, Continue)

genOwnedSticker s@(Just (path, _)) (k, v) =
    case k of
         "sticker" -> (Just (path, Sticker name value), Continue)
         "file"    -> (s, Stop)
         _         -> (s, Continue)
    where
          (name, value) = breakChar '=' v

ownedStickerFold :: ObjFold (B.ByteString, Sticker)
ownedStickerFold = parseResponse genOwnedSticker

-------------------------------------------------------------------

genElementNamed :: String -> Parser B.ByteString
genElementNamed elname Nothing (k, v) =
    if elname == k
       then (Just v, Continue)
       else (Nothing, Continue)

genElementNamed _ el _ = (el, Stop)

elementNamedFold :: String -> ObjFold B.ByteString
elementNamedFold elname = parseResponse $ genElementNamed elname

-------------------------------------------------------------------

-- | Generate Count object
genCount' :: Parser Count
genCount' c (k, v) =
    case k of
        "songs"    -> (Just $ c' { cSongs = int v }, Continue)
        "playtime" -> (Just $ c' { cPlaytime = int v }, Continue)
        _          -> (c, Continue)
    where
          c' = fromMaybe defaultCount c

defaultCount =
    Count { cSongs = -1, cPlaytime = -1 }

genCount :: ObjGen Count
genCount = parseResponse genCount' (const . return . Just) Nothing

-------------------------------------------------------------------

-- | Generate Stats object
genStats' :: Parser Stats
genStats' s (k, v) =
    case k of
        "artists"     -> (Just $ s' { stsArtists = int v }, Continue)
        "albums"      -> (Just $ s' { stsAlbums = int v }, Continue)
        "songs"       -> (Just $ s' { stsSongs = int v }, Continue)
        "uptime"      -> (Just $ s' { stsUptime = int v }, Continue)
        "playtime"    -> (Just $ s' { stsPlaytime = int v }, Continue)
        "db_playtime" -> (Just $ s' { stsDbPlaytime = int v }, Continue)
        "db_update"   -> (Just $ s' { stsDbUpdate = int v }, Continue)
        _             -> (s, Continue)
    where
          s' = fromMaybe defaultStats s

defaultStats =
     Stats { stsArtists = -1, stsAlbums = -1, stsSongs = -1, stsUptime = -1
           , stsPlaytime = -1, stsDbPlaytime = -1, stsDbUpdate = -1 }

genStats :: ObjGen Stats
genStats = parseResponse genStats' (const . return . Just) Nothing

-------------------------------------------------------------------

-- | Generate Status object
genStatus' :: Parser Status
genStatus' s (k, v) =
    case k of
       "volume"         -> (Just $ s' { stVolume = int v }, Continue)
       "repeat"         -> (Just $ s' { stRepeat = bool v }, Continue)
       "random"         -> (Just $ s' { stRandom = bool v }, Continue)
       "single"         -> (Just $ s' { stSingle = bool v }, Continue)
       "consume"        -> (Just $ s' { stConsume = bool v }, Continue)
       "playlist"       -> (Just $ s' { stPlaylistID = integer v }, Continue)
       "playlistlength" -> (Just $ s' { stPlaylistLength = int v }, Continue)
       "xfade"          -> (Just $ s' { stXFadeWidth = int v }, Continue)
       "mixrampdb"      -> (Just $ s' { stMixRampdB = frac v }, Continue)
       "mixrampdelay"   -> (Just $ s' { stMixRampDelay = frac v }, Continue)
       "state"          -> (Just $ s' { stState = state }, Continue)
       "song"           -> (Just $ s' { stSongPos = parseInt v }, Continue)
       "songid"         -> (Just $ s' { stSongID = parseInt v }, Continue)
       "time"           -> (Just $ s' { stTime = time }, Continue)
       "elapsed"        -> (Just $ s' { stTime = (frac v, snd $ stTime s') }, Continue)
       "bitrate"        -> (Just $ s' { stBitrate = int v }, Continue)
       "audio"          -> (Just $ s' { stAudio = audio }, Continue)
       "nextsong"       -> (Just $ s' { stNextSongPos = parseInt v }, Continue)
       "nextsongid"     -> (Just $ s' { stNextSongID = parseInt v }, Continue)
       "error"          -> (Just $ s' { stError = Just v }, Continue)
       _                -> (s, Continue)
    where
          s' = fromMaybe defaultStatus s
          
          integer v = fromMaybe (-1) $ parseInteger v
          frac v    = fromMaybe (read "NaN") $ parseFrac v
          audio     = fromMaybe (-1, -1, -1) $ parseTriple ':' parseInt v
          
          state =
              case B.unpack v of
                   "play"  -> Playing
                   "pause" -> Paused
                   _       -> Stopped
          
          time =
              case parseFrac *** parseInt $ breakChar ':' v of
                   (Just a, Just b) -> (a, b)
                   _                -> (-1, -1)

defaultStatus =
    Status { stState = Stopped, stVolume = -1, stRepeat = False
           , stRandom = False, stPlaylistID = -1, stPlaylistLength = -1
           , stSongPos = Nothing, stSongID = Nothing, stTime = (-1, -1)
           , stNextSongPos = Nothing, stNextSongID = Nothing
           , stBitrate = -1, stXFadeWidth = -1, stMixRampdB = -1
           , stMixRampDelay = -1, stAudio = (-1, -1, -1), stUpdatingDb = 0
           , stSingle = False, stConsume = False, stError = Nothing }

genStatus :: ObjGen Status
genStatus = parseResponse genStatus' (const . return . Just) Nothing

-------------------------------------------------------------------

-- | Generate ReplayGainMode object
genReplayGainMode :: (String, B.ByteString) -> Maybe ReplayGainMode
genReplayGainMode ("replay_gain_mode", v) =
    case B.unpack v of
         "off"   -> Just RGOff
         "track" -> Just RGTrack
         "album" -> Just RGAlbum
         _       -> Nothing

genReplayGainMode _ = Nothing

-------------------------------------------------------------------

-- | Helper conversion functions
int v  = fromMaybe (-1)  $ parseInt v
bool v = fromMaybe False $ parseBool v

