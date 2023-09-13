module Main where


import Graphics.Image.IO
import Graphics.Image       hiding ( map )

import Options.Applicative


import qualified Data.ByteString.Lazy.Char8 as BSL

import System.IO            ( hSetBuffering, stdout, BufferMode ( NoBuffering, LineBuffering, BlockBuffering ))
import Control.Monad        ( when, unless )
import Control.Concurrent   ( threadDelay, forkIO )
import System.Process       ( callCommand )
import Data.Time.Clock      ( getCurrentTime, diffUTCTime )
import System.Directory     ( listDirectory, removeFile, getTemporaryDirectory )
import Data.Either          ( isRight )
import Control.Exception    ( try, SomeException (SomeException) )



main :: IO ()
main = do
    args <- execParser opts
    
    -- line buffering is the default, but just in case
    hSetBuffering stdout LineBuffering

    tmp <- getTemporaryDirectory
    
    -- hide cursor
    putStr "\x1b[?25l"
    sTime <- getCurrentTime
    
    -- generating frames
    putStrLn "Generating frames"
    forkIO (generateFrames (tmp <> "/frames") (path args)) >> threadDelay (floor $ 2 * 10e6)
    putStrLn "Stage 1 done"

    -- preparing frames
    raw <- takeFrames (tmp <> "/frames") 1 (skip args) (resolution args)
    putStrLn "Stage 2 done, complete"

    -- displaying frames
    let frames = map (compress . pairs . toLists) raw
    doAtLeastOnce (loop args) (displayLoop frames)

    -- show cursor
    putStr $ clearColour <> "\x1b[?25h"

    unless (dontClearTemp args) $ do
        eTime <- getCurrentTime

        fs <- listDirectory (tmp <> "/frames")
        
        putStr $ "displayed " <> show (length fs `div` skip args) <> " frames in " <> show (diffUTCTime eTime sTime)
        putStr "\nClearing Temp Directory"
        listDirectory (tmp <> "/frames/") >>= mapM_ (removeFile . ((tmp <> "/frames/") ++) )

    where
        opts :: ParserInfo Args
        opts = info (argPars <**> helper)
            ( fullDesc
          <> progDesc "fully terminal based media player"
            )

        doAtLeastOnce :: Bool -> IO () -> IO ()
        doAtLeastOnce b f = do
            f
            when b $ doAtLeastOnce b f

            



displayLoop :: [ [[(Pixel RGB Double, Pixel RGB Double)]] ] -> IO ()
displayLoop []          = return ()
displayLoop (frame:xs)  = do
    putStr "\x1b[H" -- cursor to home position
    
    displayFrame frame
    
    displayLoop xs

displayFrame :: [ [(Pixel RGB Double, Pixel RGB Double)] ] -> IO ()
displayFrame = mapM_ (\x -> displayRow x >> putStr ("\n" <> clearColour)) 

displayRow :: [ (Pixel RGB Double, Pixel RGB Double) ] -> IO ()
displayRow = mapM_ 
    (\(x, y) -> do
        let fg = addFgColour (pixelToColour x)
        let bg = addBgColour (pixelToColour y)

        putStr $ fg <> bg <> "▀"
    )

-- genRow :: [ (Pixel RGB Double, Pixel RGB Double) ] -> BSL.ByteString
-- genRow = concat . map (\(x, y) -> addFgColour (pixelToColour x) <> addBgColour (pixelToColour y) <> "▀")

takeFrames :: String -> Int -> Int -> (Int, Int) -> IO [Image VS RGB Double]
takeFrames tmp n s xy = do
    eImg <- try $ readImage' (tmp <> "/frame" <> show n <> ".png") :: IO (Either SomeException (Image VS RGB Double))
    case eImg of
        Left _      -> return []
        Right image -> do
            rest <- takeFrames tmp (n + s) s xy
            return (scaleToFit image xy : rest)


generateFrames :: String -> FilePath -> IO ()
generateFrames tmp path = callCommand $ "ffmpeg -hide_banner -loglevel error -i " <> path <> " " <> tmp <>"/frame%d.png"


-- pair up two neighbouring rows of pixels without repetition
compress :: [([a], [b])] -> [[(a, b)]]
compress []          = []
compress ((a, b):xs) = zip a b : compress xs

pairs :: [[a]] -> [([a], [a])]
pairs []          = []
pairs (x:y:xs)    = (x,y) : pairs xs
pairs (x:xs)      = (x,x) : pairs xs


pixelToColour :: Pixel RGB Double -> Colour
pixelToColour (PixelRGB r1 g1 b1) = (r2, g2, b2)
    where
        r2 = show $ floor (r1 * 255)
        g2 = show $ floor (g1 * 255)
        b2 = show $ floor (b1 * 255)

addFgColour :: Colour -> String
addFgColour (r, g, b) = "\x1b[38;2;" <> r <> ";" <> g <> ";" <> b <> "m"

addBgColour :: Colour -> String
addBgColour (r, g, b) = "\x1b[48;2;" <> r <> ";" <> g <> ";" <> b <> "m"

clearColour :: String
clearColour = "\x1b[0m"


type Colour = (String, String, String)


scaleToFit :: Image VS RGB Double -> (Int, Int) -> Image VS RGB Double
scaleToFit img (x, y)
    | min ry rx < 1 = scale Bilinear Edge (rx, ry) img 
    | otherwise     = img
    where
        rs = fromIntegral $ rows img
        cs = fromIntegral $ cols img

        rx = fromIntegral x / rs
        ry = fromIntegral y / cs


argPars :: Parser Args
argPars = Args
    <$> strOption
        ( long "path"
       <> short 'p'
       <> help "path to media"
        )
    <*> switch
        ( long "loop"
       <> short 'l'
       <> help "turn on loop for video media"
        )
    <*> switch
        ( long "dontClearTemp"
       <> short 'c'
       <> help "dont clean temporary files"
        )
    <*> option auto
        ( long "skip"
       <> short 's'
       <> help "skip every N frames of video media"
       <> value 2
        )
    <*> option auto
        ( long "resolution"
       <> short 'r'
       <> help "maximum resolution to try and match, (144, 192) by default"
       <> value (144, 192)
        )


data Args = Args
    { path          :: String
    , loop          :: Bool
    , dontClearTemp :: Bool
    , skip          :: Int
    , resolution    :: (Int, Int)
    }
