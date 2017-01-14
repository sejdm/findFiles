module Main where

import FileSearch
import System.Environment
import qualified Data.Map as M
import System.Console.ANSI

screenmplayer = "screen -d -m mplayer"
screenmplayerpls = "screen -d -m mplayer -playlist"

fileTypes :: FileAssociation
fileTypes = M.fromList [
    ("mp3",screenmplayer)
  , ("mp4",screenmplayer)
  , ("avi", screenmplayer)
  , ("mkv", screenmplayer)
  , ("wav", screenmplayer)
  , ("flv", screenmplayer)
  , ("webm", screenmplayer)
  , ("pls", screenmplayerpls)
  , ("pdf", "zathura")
  , ("djvu", "zathura")
  , ("epub", "ebook-viewer")
  , ("ps", "zathura")
  , ("dvi", "zathura")
  , ("cbz", "mcomix")
  , ("cbr", "mcomix")
  , ("tex", "emacs24")
                   ]

fileDB = "/home/shane/db/files" 

mySettings = defaultSettings {
    listFgColour = (Vivid, Blue)
  , queryFgColour = (Vivid, Yellow)
  , errorFgColour = (Vivid, Red)
  , listBgColour = (Dull, Black)
  , queryBgColour = (Dull, Black)
  , errorBgColour = (Dull, Black)
                             }


main :: IO ()
main = do as <- getArgs
          opeFile mySettings fileDB fileTypes (unwords as)
