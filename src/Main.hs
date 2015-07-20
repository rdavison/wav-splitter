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
import System.Directory
import Data.List
import Data.List.Split
import Control.Concurrent.Async (mapConcurrently)
import System.Environment (getArgs)


pauseSampleLength = 82000
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

mkOutFilename :: String -> Int -> FilePath
mkOutFilename base n = "output/" ++ base ++ "-" ++ show n ++ ".wav"

mkCombinedFilename :: String -> String -> FilePath
mkCombinedFilename left right = "output/" ++ "left" ++ "-" ++ "right" ++ ".wav"

saveResults :: [FilePath] -> [Audio Word8] -> IO ()
saveResults splitFilename audioList = zipWithM_ exportFile outputFilenames audioList
    where
        outputFilenames = fmap (mkOutFilename base) [1..]
        name = head $ splitOn "." (splitFilename !! 2)
        base = concat $ intersperse "/" [splitFilename !! 1, name]

saveCombinedResults :: String -> String -> Audio Word8 -> IO ()
saveCombinedResults left right audio
    = exportFile outputFilename audio
    where
        outputFilename = mkCombinedFilename left right
        --let pair = take 2 filenames
        --let mapped = map parseFilename pair
        --where
        --    name = head $ splitOn "." (splitFilename !! 2)
        --    base = concat $ intersperse "/" [splitFilename !! 1, name]

toAudio :: Int -> Int -> BS.ByteString -> Audio Word8
toAudio sampleRate' channelNumber' rawResults
    = Audio
    {   sampleRate    = sampleRate'
    ,   channelNumber = channelNumber'
    ,   sampleData    = toSampleData . toList $ rawResults }
    where
        toList = BS.unpack
        toSampleData = \x -> array (0, (length x) - 1) (zip [0..] x)

parseFile :: [String] -> IO ()
parseFile splitFilename = do
    let inputFilename = concat $ intersperse "/" splitFilename

    -- Read the file
    putStrLn $ "Reading in file " ++ inputFilename
    rawAudio <- readWavFile inputFilename
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
            saveResults splitFilename $ rebuild results
            putStrLn "Done"

        Left errorMsg -> do
            putStrLn "Parser returned an error..."
            putStrLn . show $ errorMsg

getFilenames :: IO [FilePath]
getFilenames = do
    -- Specify the file we want to parse
    let filename = "res/GLOSSIKA-ENYUE-F1-GMS-0001A.wav"
    let foldername = "res/"
    rawFolders <- fmap (filter (\x -> not $ isPrefixOf "." x)) $ getDirectoryContents foldername
    let folders' = fmap (foldername ++) rawFolders
    let folders = fmap (\x -> x ++ "/") folders'
    rawListOfFiles <- mapM getDirectoryContents folders
    let filteredRawListOfFiles = fmap (filter (\x -> not $ isPrefixOf "." x)) rawListOfFiles
    let rawListOfFiles' = zip folders filteredRawListOfFiles
    let listOfFiles = fmap (\(folder, files) -> map (folder ++) files) rawListOfFiles'
    let flattened = concat listOfFiles
    return flattened

appSplitWavs :: IO ()
appSplitWavs = do
    filenames <- getFilenames
    let splitFilenames = fmap (splitOn "/") (take 1 (filter (isInfixOf "ENKR") filenames))
    mapM parseFile splitFilenames >> return ()

appJoiner :: FilePath -> FilePath -> IO ()
appJoiner left right = do
    leftAudio <- readWavFile left
    rightAudio <- readWavFile right
    return ()

    ---- Convert to ByteString for Attoparsec parsing
    --let leftByteString = BS.pack .  elems .  sampleData $ leftAudio
    --let rightByteString = BS.pack .  elems .  sampleData $ rightAudio
    --let combinedByteString = leftByteString `BS.append` rightByteString
    --let combinedAudio = toAudio (sampleRate leftAudio) (channelNumber leftAudio) combinedByteString
    --saveCombinedResults left right combinedAudio

main :: IO ()
main = undefined
