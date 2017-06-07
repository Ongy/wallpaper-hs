module X11
    ( Monitor
    , getMonitors
    )
where

import Control.Exception (bracket)
import System.Environment (getEnv)
import Graphics.X11.Xlib (Display, ScreenNumber, RRCrtc)

import Graphics.X11.Xrandr
    ( xrrGetScreenResources
    , xrrGetCrtcInfo
    , XRRScreenResources(..)
    , XRRCrtcInfo(..)
    )

import Graphics.X11.Xlib.Display
    ( openDisplay
    , closeDisplay
    , screenCount
    , rootWindow
    )

import System.IO

type Monitor = (Word, Word)

withDisplay :: String -> (Display -> IO a) -> IO a
withDisplay dsp fun =
    bracket (openDisplay dsp) closeDisplay fun

withDefaultDisplay :: (Display -> IO a) -> IO a
withDefaultDisplay fun = do
    name <- getEnv "DISPLAY"
    withDisplay name fun

crtcToMon :: Display -> XRRScreenResources -> RRCrtc -> IO Monitor
crtcToMon dsp res crtc = do
    (Just info) <- xrrGetCrtcInfo dsp res crtc
    pure $ ( fromIntegral $ xrr_ci_width  info
           , fromIntegral $ xrr_ci_height info)

getScreenMons :: Display -> ScreenNumber -> IO [Monitor]
getScreenMons dsp num = do
    root <- rootWindow dsp num
    (Just rrs) <- xrrGetScreenResources dsp root
    mapM (crtcToMon dsp rrs) $ xrr_sr_crtcs rrs

getMonitors :: IO [Monitor]
getMonitors = withDefaultDisplay $ \dsp -> do
    let count = fromIntegral $ screenCount dsp
    mons <- mapM (getScreenMons dsp) [0 .. count - 1]
    pure $ concat mons
