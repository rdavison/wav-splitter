{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Wav (importFile, exportFile)
import qualified Codec.ByteString.Parser as CBP
import Control.Applicative (many)
import Control.Monad (zipWithM_)
import Data.Array.IArray (elems)
import Data.Attoparsec.Binary (anyWord16le)
import Data.Attoparsec.ByteString.Lazy (Parser, Result(..), parse)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Combinator (lookAhead, count)
import Data.Audio (Audio(..), SampleData(..), sampleData, parseSampleData)
import Data.ByteString.Lazy (ByteString(..))
import Data.ByteString.Builder (int16LE, toLazyByteString)
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Int (Int16)
--import Prelude hiding (takeWhile, take)
import System.Environment (getArgs)
import System.FilePath (splitExtension, joinPath, splitDirectories)


toByteString :: Audio Int16 -> ByteString
toByteString = toLazyByteString . foldMap int16LE . elems . sampleData

toByteString' :: Audio Int16 -> ByteString
toByteString' = toLazyByteString . foldMap int16LE . elems . sampleData


parseSilenceByte :: Parser Int16
parseSilenceByte = do
    w <- anyWord16le
    let w' = fromIntegral w
    if w' >= -128 && w' <= 128 then
        return w'
    else
        fail $ "parseSilenceByte"

parseNoiseByte :: Parser Int16
parseNoiseByte = do
    w <- anyWord16le
    let w' = fromIntegral w
    if w' < -128 || w' > 128 then
        return w'
    else
        fail $ "parseNoiseByte"


parseChunkStart :: Parser Int
parseChunkStart = go 0 where
    go i = do
        chunk <- lookAhead $ many parseSilenceByte
        let chunkLen = length chunk
        if chunkLen == 0 then do
            DAB.take 2
            go (i+2)
        else do
            DAB.take (chunkLen * 2)
            return $ i + chunkLen


parseChunkEnd :: Parser Int
parseChunkEnd = goNoise 0 where
    goSilence i = do
        silenceChunk <- lookAhead $ many parseSilenceByte
        let chunkLen = length silenceChunk
        if chunkLen > 44100 then do
            return i
        else do
            DAB.take (chunkLen * 2)
            goNoise $ i + chunkLen

    goNoise i = do
        noiseChunk <- lookAhead $ many parseNoiseByte
        let chunkLen = length noiseChunk
        if chunkLen == 0 then do
            DAB.take 2
            goNoise $ i + 2
        else do
            DAB.take (chunkLen * 2)
            goSilence $ i + chunkLen


parseChunkIndex :: Parser (Int, Int)
parseChunkIndex = do
    chunkStart <- parseChunkStart
    chunkEnd <- parseChunkEnd
    return (chunkStart, chunkEnd)

parseChunkIndices :: Parser [(Int, Int)]
parseChunkIndices = many parseChunkIndex
        

-- TODO: Fix this function to allow for arbitrary input/output folders
getOutputFilePath :: FilePath -> Int -> FilePath
getOutputFilePath fp num =
    let (base, ext) = splitExtension . joinPath . ("out" :) . tail . splitDirectories $ fp
    in base ++ "-" ++ (show num) ++ ext


extractChunk :: Int -> Int -> CBP.Parser (SampleData Int16)
extractChunk skipBytes takeBytes = do
    _ <- parseSampleData skipBytes CBP.getInt16le
    parseSampleData takeBytes CBP.getInt16le
    

extractChunks :: [(Int, Int)] -> CBP.Parser [SampleData Int16]
extractChunks xs = mapM (uncurry extractChunk) xs


sliceAudio :: Audio Int16 -> [(Int, Int)] -> [Audio Int16]
sliceAudio audio indices = either (const []) audioSlices parseResults
    where
        parseResults = CBP.runParser (extractChunks indices) (toByteString' audio)
        audioSlices = fmap (\sd -> Audio (sampleRate audio) (channelNumber audio) sd)


process :: FilePath -> Audio Int16 -> IO ()
process fp audio = do
    case parse parseChunkIndices (toByteString audio) of
        Fail rem ctxs err -> do
            putStrLn err
      
        Done rem indexPairs -> do
            putStrLn "Success..."
            putStrLn . show $ indexPairs
            let slices = sliceAudio audio indexPairs -- (take 5 indexPairs) -- [indexPairs !! 0, indexPairs !! 1]
            let ofps = zipWith getOutputFilePath (repeat fp) [1..]
            zipWithM_ exportFile ofps slices
            --mapM_ (putStrLn.show.elems.sampleData) slices


runApp :: FilePath -> IO ()
runApp fp = putStrLn ("Reading File: " ++ fp) >> importFile fp >>= either skip (process fp)
    where
        skip :: String -> IO ()
        skip msg = putStrLn $ "Error: " ++ msg ++ "... skipping file..."


main :: IO ()
main = do
    args <- getArgs
    mapM_ runApp args
    return ()
