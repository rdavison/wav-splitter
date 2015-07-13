module Main where

import Codec.Wav (importFile, exportFile)
import Data.Array.Unboxed (elems)
import System.IO (FilePath)
import Data.Audio
import Data.Array.IArray
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as A
import Control.Applicative
import Control.Monad (zipWithM_)
import Control.Monad.Loops (untilM)

glossika :: FilePath
glossika = "res/GLOSSIKA-ENJA-F1-GMS-0001A.wav"

getAudio :: FilePath -> IO (Audio Word8)
getAudio path = do
    maybeAudio <- importFile path
    case maybeAudio of
        Left s -> undefined
        Right t -> return t
    
isSilence :: Word8 -> Bool
isSilence x = and checkResults where
    lowerBound = 128
    upperBound = 128
    checkResults = do
        f <- [(lowerBound >=), (upperBound <=)]
        return $ f x
        
noise1 :: A.Parser BS.ByteString
noise1 = A.takeWhile $ not . isSilence

mySilence :: A.Parser (Maybe BS.ByteString)
mySilence = go 0 BS.empty where
    go i acc = do
        x <- A.peekWord8
        case x of
            Just x' -> if or [x' == 126, x' == 127, x' == 128, x' == 129, x' == 130]
                           then do
                               A.take 1
                               go (i+1) $ x' `BS.cons` acc
                           else if i < 20500
                                    then return $ Just acc
                                    else return $ Nothing
            Nothing -> return $ Nothing

myChunk :: A.Parser BS.ByteString
myChunk = go BS.empty where
    go acc = do
        noisyChunk <- noise1
        silentChunk <- mySilence
        case silentChunk of
            Just chunk -> go $ acc' where acc' = acc `BS.append` (noisyChunk `BS.append` chunk)
            Nothing    -> return $ acc `BS.append` noisyChunk

parseGlossika :: A.Parser [BS.ByteString]
parseGlossika = myChunk `untilM` (A.atEnd)

getFilename :: Int -> FilePath
getFilename n = "output/out-" ++ show n ++ ".wav"

main :: IO ()
main = do
    -- Get the audio and print stats
    rawAudio <- getAudio glossika
    putStrLn . show $ rawAudio

    -- Convert to bytestring for further processing
    let bs = BS.pack .  elems .  sampleData $ rawAudio
    putStrLn . show $ BS.length bs
    case A.parseOnly (A.skipWhile (== 128) *> parseGlossika) bs of
        Right s -> do 
            let asAudio = fmap (toAudio . toSampleData . toList) s
            zipWithM_ exportFile (fmap getFilename [1..]) asAudio
            where
                toList = BS.unpack
                toSampleData = \x -> array (0, (length x) - 1) (zip [0..] x)
                toAudio = \x -> Audio (sampleRate rawAudio) (channelNumber rawAudio) x
        Left s -> putStrLn . show $ s
