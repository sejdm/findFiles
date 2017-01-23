module Main where

import FileSearch
import System.Environment
import System.Console.ANSI
import Data.Monoid


fileTypes = usingMonoid $
     simpleCommand "zathura" ["pdf", "djvu", "ps", "dvi"]
  <> simpleCommand "screen -d -m mplayer -input file=/home/shane/mplayerfifo" ["mp3", "mp4", "webm", "flv", "wav", "avi"]
  <> simpleCommand "screen -d -m mplayer -playlist -input file=/home/shane/mplayerfifo" ["pls"]
  <> simpleCommand "mcomix" ["cbr", "cbz"]
  <> simpleCommand "emacs24" ["tex", "hs"]
  <> simpleCommand "ebook-viewer" ["epub"]
  <> simpleCommand "chromium-browser" ["html"]


fileDB = "/home/shane/db/files"
printQFile = "/home/shane/printQ"

mySettings = defaultSettings {
    listColour = fg Vivid Blue <> bg Dull Black
  , queryColour = fg Vivid Yellow
  , errorColour = fg Vivid Red
  , loadEntireDatabase = True
                             }


main :: IO ()
main = do a@(n:m) <- getArgs
          if n /= "p" then opeFile mySettings fileDB fileTypes ( unwords a) else  printQ mySettings fileDB printQFile (unwords m)
