module Main where

import Codec.Wav (importFile, bitsPerSample)
import Data.Audio (Audio(Audio))
import Data.Array.Unboxed (elems)
import Data.Int (Int32)
import System.IO (FilePath)
import Data.Audio
import Data.Array.IArray
import Data.Word (Word8)
import Data.ByteString (pack, ByteString)
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as A
import Control.Applicative
import Numeric (showHex)
import Data.List (sort, reverse)
import Data.Bool (bool)
import Control.Monad.Loops (untilM)

glossika :: FilePath
glossika = "res/GLOSSIKA-ENJA-F1-GMS-0001A.wav"

getAudio :: FilePath -> IO (Audio Word8)
getAudio path = do
    maybeAudio <- importFile path
    case maybeAudio of
        Left s -> undefined
        Right t -> return t
    
silent = 10
numSeconds = 1
secondsToSamples seconds audio = sampleRate audio * seconds

printStats audio = do
    putStrLn . ("Sample Rate: " ++) . show . sampleRate $ audio
    putStrLn . ("Channel Number: " ++) . show . channelNumber $ audio
    putStrLn . ("Bits Per Sample: " ++) . show . bitsPerSample $ audio

-- parseChunk :: A.Parser ByteString
-- parseChunk = do
--     silent <- A.takeWhile1 (\x -> and [(x >= lowerBound), (x <= upperBound)])
--     noisy <- A.takeWhile1 (\x -> or [(x <  lowerBound), (x > upperBound)])
--     if BS.length silent < 41000
--         then return $ silent `BS.append` noisy
--         else return noisy
--     where
--         lowerBound = 128
--         upperBound = 128
--     --return (BS.length thingy1, BS.length thingy2)

parseChunk :: A.Parser ByteString
parseChunk = do
    front <- A.peekWord8
    case front of
        Just a -> if a == 128 
            then A.skipWhile (== 128) >> go BS.empty
            else go BS.empty
        Nothing -> return $ (BS.pack [])
    where
        lowerBound = 128
        upperBound = 128

        go :: ByteString -> A.Parser ByteString
        go acc = do
            noisy <- A.takeWhile (\x -> and [(x >= lowerBound), (x <= upperBound)])
            silent <- A.takeWhile (\x -> or [(x < lowerBound), (x > upperBound)])
            if BS.length silent < 1000
                then go $ acc `BS.append` (noisy `BS.append` silent)
                else return $ acc `BS.append` noisy

        


skipSilence = A.manyTill (A.satisfy $ const True) (A.satisfy (/= 128))

isSilence :: Word8 -> Bool
isSilence x = and checkResults where
    lowerBound = 128
    upperBound = 128
    checkResults = do
        f <- [(lowerBound >=), (upperBound <=)]
        return $ f x
        
noise = A.takeWhile $ not . isSilence
noise1 = A.takeWhile $ not . isSilence
silence = A.takeWhile isSilence
silence1 = A.takeWhile1 isSilence

getChunk :: A.Parser ByteString
getChunk = go BS.empty where 
    go acc = do
        noisyChunk <- noise1
        end <- A.atEnd
        if end
            then do
                silentChunk <- silence
                return $ acc `BS.append` noisyChunk 
            else do
                silentChunk <- silence
                let silenceLength = BS.length silentChunk
                end' <- A.atEnd
                if end'
                    then return $ acc `BS.append` noisyChunk
                    else if silenceLength < 1000
                        -- keep going
                        then go $ acc `BS.append` (noisyChunk `BS.append` silentChunk)
                        -- stop
                        else return $ acc `BS.append` noisyChunk

data SilenceType a = Internal a | Gap a

mySilence :: A.Parser (Maybe ByteString)
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

myChunk :: A.Parser ByteString
myChunk = go BS.empty where
    go acc = do
        noisyChunk <- noise1
        silentChunk <- mySilence
        case silentChunk of
            Just chunk -> go $ acc' where acc' = acc `BS.append` (noisyChunk `BS.append` chunk)
            Nothing    -> return $ acc `BS.append` noisyChunk
        --if BS.length silentChunk < 1000
        --    then if BS.length noisyChunk < 1
        --        then return BS.empty
        --        else go $ acc `BS.append` (noisyChunk `BS.append` silentChunk)
        --    else return $ acc `BS.append` noisyChunk
    

--parseGlossika :: A.Parser [ByteString]
--parseGlossika = silence *> getChunk `A.sepBy` silence1
parseGlossika = myChunk `untilM` (A.atEnd)

buildAudio rawChunk = Audio 
                               
main = do
    -- Get the audio and print stats
    rawAudio <- getAudio glossika
    putStrLn . show $ rawAudio

    -- Convert to bytestring for further processing
    let bs = pack .  elems .  sampleData $ rawAudio
    putStrLn . show $ BS.length bs
    --A.parseTest (parseGlossika <* A.endOfInput) bs
    case A.parseOnly (A.skipWhile (== 128) *> parseGlossika) bs of
        Right s -> do
            let chunkSizes = fmap BS.length s
            let numChunks = length chunkSizes
            -- --putStrLn . show $ "Chunks: " ++ show s
            -- --putStrLn . show $ "Num Chunks: " ++ show numChunks
            putStrLn . show $ "Chunk Sizes: " ++ show chunkSizes
            putStrLn . show $ "Num Chunks: " ++ show numChunks
        Left s -> putStrLn . show $ s
