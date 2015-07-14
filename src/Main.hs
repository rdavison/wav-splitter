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

pauseSampleLength = 20500
deadSilence = 128
(silenceRangeStart, silenceRangeEnd) = (126, 130)

readWavFile :: FilePath -> IO (Audio Word8)
readWavFile path = do
    maybeAudio <- importFile path
    case maybeAudio of
        Left s -> undefined
        Right t -> return t
    
isSilence :: Word8 -> Bool
isSilence x = x == deadSilence
        
parseSoundChunk :: A.Parser BS.ByteString
parseSoundChunk = A.takeWhile $ not . isSilence

parseMaybeSilentChunk :: A.Parser (Maybe BS.ByteString)
parseMaybeSilentChunk = go 0 BS.empty where
    go i acc = do
        x <- A.peekWord8
        case x of
            Nothing -> return Nothing
            Just x' ->
                if or $ fmap (x' ==) [silenceRangeStart..silenceRangeEnd]
                    then do
                        A.take 1 -- consume the byte
                        go (i+1) $ x' `BS.cons` acc
                    else if i < pauseSampleLength
                        then return $ Just acc
                        else return Nothing

parseChunk :: A.Parser BS.ByteString
parseChunk = go BS.empty where
    go acc = do
        soundChunk <- parseSoundChunk
        maybeSilentChunk <- parseMaybeSilentChunk
        case maybeSilentChunk of
            Nothing           -> return $ acc `BS.append` soundChunk
            Just silentChunk' -> go $ acc' where acc' = acc `BS.append` (soundChunk `BS.append` silentChunk')

parseSplitOnPause :: A.Parser [BS.ByteString]
parseSplitOnPause = parseChunk `untilM` (A.atEnd)

mkOutFilename :: Int -> FilePath
mkOutFilename n = "output/out-" ++ show n ++ ".wav"

saveResults :: [Audio Word8] -> IO ()
saveResults = zipWithM_ exportFile outputFilenames
    where
        outputFilenames = fmap mkOutFilename [1..]

toAudio :: Int -> Int -> BS.ByteString -> Audio Word8
toAudio sampleRate' channelNumber' rawResults
    = Audio
    {   sampleRate    = sampleRate'
    ,   channelNumber = channelNumber'
    ,   sampleData    = toSampleData . toList $ rawResults }
    where
        toList = BS.unpack
        toSampleData = \x -> array (0, (length x) - 1) (zip [0..] x)

main :: IO ()
main = do
    -- Specify the file we want to parse
    let filename = "res/GLOSSIKA-ENJA-F1-GMS-0001A.wav"

    -- Read the file
    putStrLn $ "Reading in file " ++ filename
    rawAudio <- readWavFile filename
    putStrLn ""

    -- Print information about the parsed file
    putStrLn "Finished reading file"
    putStrLn "---------------------"
    putStrLn . show $ rawAudio

    -- Convert to ByteString for Attoparsec parsing
    let rawByteString = BS.pack .  elems .  sampleData $ rawAudio

    -- Fast-forward parser to the good parts
    let parseSplitOnPause' = A.skipWhile isSilence *> parseSplitOnPause

    -- Run the Parser
    let parseResults = A.parseOnly parseSplitOnPause' rawByteString

    -- Create a function to rebuild the raw parsed results
    let rebuild = fmap $ toAudio (sampleRate rawAudio) (channelNumber rawAudio)

    -- Check the results
    case parseResults of
        Right results -> do
            putStrLn "Parser success! Saving results..."
            saveResults $ rebuild results
            putStrLn "Done"

        Left errorMsg -> do
            putStrLn "Parser returned an error..."
            putStrLn . show $ errorMsg
