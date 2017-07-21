module Main where

import FileSearch
import System.Environment
import System.Console.ANSI
import Data.Monoid


fileTypes = usingMonoid $
    "zathura" .= ["pdf", "djvu", "ps", "dvi"]
  -- <> "screen -d -m mpv -input file=/home/shane/mplayerfifo" .= ["mp3", "mp4", "webm", "flv", "wav", "avi"]
  <> "tmux new -s Media -d mpv --audio-display=no -input file=/home/shane/mplayerfifo" .= ["mp3", "mp4", "webm", "flv", "wav", "avi"]
  <> "tmux new -s Media -d mpv --audio-display=no -input file=/home/shane/mplayerfifo -playlist" .= ["pls"]
  <> "mcomix" .= ["cbr", "cbz"]
  <> "emacs24" .= ["tex", "hs"]
  <> "ebook-viewer" .= ["epub", "mobi"]
  <> "chromium-browser" .= ["html"]


fileDB = "/home/shane/db/files"
printQFile = "/home/shane/printQ"

mySettings = defaultSettings {
    listColour = fg Dull Blue
  , queryColour = fg Dull Green
  , errorColour = fg Vivid Red
  , loadEntireDatabase = True
                             }


main :: IO ()
main = do a@(n:m) <- getArgs
          if n /= "p" then opeFile mySettings fileDB fileTypes ( unwords a) else  printQ mySettings fileDB printQFile (unwords m)
