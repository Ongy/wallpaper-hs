{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import GHC.Generics

import System.Process (callProcess)
import System.Random (randomRIO)
import Control.Monad (when)
import Data.List ((\\))
import Data.Store (decodeEx, Store, encode)
import System.IO (hPutStrLn, hPutStr, stderr, withFile, IOMode(..))
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import qualified Codec.Picture as P
import System.Directory
    ( setCurrentDirectory
    , getHomeDirectory
    , doesFileExist
    , doesDirectoryExist
    , listDirectory
    )

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Config
import Config.Schema

import System.Environment.XDG.BaseDir (getUserConfigFile, getUserDataDir)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Map (Map)
import qualified Data.Map as M

import X11

data Image
  = Image
    { imReadable :: Bool
    , imWidht :: Word
    , imHeight :: Word
    } deriving(Generic, Show, Eq)

instance Store Image

type DB = Map String Image

dbIOPath :: IO String
dbIOPath = (++ "/wallpaper.db") <$> getUserDataDir "wallpaper-hs"

dbPath = unsafePerformIO dbIOPath

pathConfig :: IO String
pathConfig = getUserConfigFile "wallpaper-hs" "config"

wallpaperSpec :: ValueSpecs Text
wallpaperSpec = sectionsSpec "top-level configuration" $
    reqSection "wallpaper-dir" "The directory to search for possible wallpapers"

getWallpaperPath :: IO String
getWallpaperPath = do
    conf <- pathConfig
    res <- T.readFile conf
    let values = case parse res of
            Left x -> error $ show x
            Right x -> x
    case loadValue wallpaperSpec values of
        Left x -> error $ show x
        Right y -> return $ T.unpack y

changeToDir :: IO ()
changeToDir = do
  args <- getArgs
  dir <- getWallpaperPath
  hPutStrLn stderr ("Using \"" ++ dir ++ "\" for operation")
  setCurrentDirectory dir

readDBI :: IO DB
readDBI = withFile dbPath ReadMode $ fmap decodeEx . BS.hGetContents

readDB :: IO DB
readDB = do
  exists <- doesFileExist dbPath
  if exists
    then readDBI
    else return mempty

writeDB :: DB -> IO ()
writeDB db = withFile dbPath WriteMode $ flip BS.hPutStr (encode db)

listDirectoryRecursive :: String -> IO [String]
listDirectoryRecursive dir = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      dirs <- map ((++) (dir ++ "/")) <$> listDirectory dir
      concat <$> mapM listDirectoryRecursive dirs
    else do
      fexists <- doesFileExist dir
      if fexists
        then return [dir]
        else error ("Coult not find directory: " ++ dir)

addImage :: String -> IO DB -> IO DB
addImage imagePath iodb = do
    image <- P.readImage imagePath
    db <- iodb
    return $ case image of
        (Left _) -> M.insert imagePath (Image False 0 0) db
        (Right x) -> let im = P.convertRGB8 x
                         height = fromIntegral $ P.imageHeight im
                         width = fromIntegral $ P.imageWidth im
                      in M.insert imagePath (Image True width height) db

suitableForMon :: DB -> Monitor -> DB
suitableForMon db (mw, mh) = M.filter isOK db
  where isOK (Image _ iw ih) = let w = fromIntegral iw / fromIntegral mw
                                   h = fromIntegral ih / fromIntegral mh
                                   rel = if w > h then (w - h) / w else (h - w) / h in
                                     trace (show rel) rel < 0.10


chooseRandom :: DB -> IO String
chooseRandom db = do
  let list = M.toList db
  index <- randomRIO (0, length list - 1)
  return . fst $ (list !! index)


setImages :: [String] -> IO ()
setImages xs = callProcess "feh" ("--image-bg": "black": "--bg-max": xs)


main :: IO ()
main = do
  changeToDir
  db <- readDB
  mons <- getMonitors
  hPutStr stderr "Working with monitors: "
  hPutStrLn stderr $ show mons

  let keys = M.keys db
  files <- listDirectoryRecursive "."
  let new = files \\ keys

  ndb <- foldr addImage (return $ M.filterWithKey (\k _ -> k `elem` files) db) new

  let images = M.filter imReadable ndb

  let suitables = map (suitableForMon images) mons

  chosen <- mapM chooseRandom suitables

  hPutStrLn stderr $ show chosen
  setImages chosen

  when (ndb /= db) (hPutStrLn stderr "Writing new DB" >> writeDB ndb)
