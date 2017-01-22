module Main where

import FileSearch
import System.Environment
import System.Console.ANSI
import Data.Monoid


fileTypes :: FileAssociation
fileTypes =
     shComm "zathura" ["pdf", "djvu", "ps", "dvi"]
  <> shComm "screen -d -m mplayer" ["mp3", "mp4", "webm", "flv", "wav", "avi"]
  <> shComm "screen -d -m mplayer -playlist" ["pls"]
  <> shComm "mcomix" ["cbr", "cbz"]
  <> shComm "emacs24" ["tex", "hs"]
  <> shComm "ebook-viewer" ["epub"]
  <> shComm "chromium-browser" ["html"]


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
