{- |
Module      :  fastaYX2XY.hs
Description :  Progam to take fasta files by locus and return fasta files by taxon (ie file) name 
Copyright   :  (c) 2021 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
License     :

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies,
either expressed or implied, of the FreeBSD Project.

Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
Portability :  portable (I hope)

-}

module Main where

--import           Debug.Trace
import           System.IO
import System.Environment
import           Data.List
import           Types.Types
import qualified Data.Text.Lazy  as T
import qualified Data.Text.Short as ST


-- | checkDuplicatedTerminals takes list TermData and checks for repeated terminal names
checkDuplicatedTerminals :: [TermData] -> (Bool, [T.Text]) 
checkDuplicatedTerminals inData =
    if null inData then (False, []) 
    else 
        let nameList = group $ sort $ fmap fst inData
            dupList = filter ((>1).length) nameList
        in
        if null dupList then (False, [])
        else (True, fmap head dupList)

-- | getRawDataPairsFastA takes splits of Text and returns terminalName, Data pairs--minimal error checking
getRawDataPairsFastA :: String -> [T.Text] -> String -> [TermData]
getRawDataPairsFastA modifier inTextList fileName =
    if null inTextList then []
    else 
        let firstText = head inTextList
            --firstName = T.append (T.pack fileName) $ T.takeWhile (/= '$') $ T.takeWhile (/= ';') $ head $ T.lines firstText
            firstlocus = T.reverse $ T.takeWhile (/= '|') $ T.reverse $ head $ T.lines firstText
            firstName = T.append (T.takeWhile (/='.') $ T.pack fileName) (T.cons '$' firstlocus)
            firstData = T.filter (/= ' ') $ T.toUpper $ T.concat $ tail $ T.lines firstText
            firstDataNoGaps = T.filter (/= '-') firstData
            firtDataSTList = ST.fromText $ T.toStrict firstData
            firstDataNoGapsSTList = ST.fromText $ T.toStrict firstDataNoGaps
        in
        --trace (T.unpack firstName ++ "\n"  ++ T.unpack firstData) (
        --trace ("FA " ++ (show $ length firstDataNoGapsSTList)) (
        if modifier == "prealigned" then (firstName, firtDataSTList) : getRawDataPairsFastA modifier (tail inTextList) fileName
        else (firstName, firstDataNoGapsSTList) : getRawDataPairsFastA modifier (tail inTextList) fileName
        --)

-- | getFastA processes fasta file 
-- assumes single character alphabet
-- deletes '-' (unless "prealigned"), and spaces
getFastA :: String -> (String, String) -> [TermData] 
getFastA modifier (fileContents', fileName)  =
    if null fileContents' then errorWithoutStackTrace ("\n\n'Read' command error: empty file")
    else 
        -- removes ';' comments   
        let fileContents =  unlines $ filter (not.null) $ fmap (takeWhile (/= ';')) $ lines fileContents'
        in 
        if (head fileContents) /= '>' then errorWithoutStackTrace ("\n\n'Read' command error: fasta file must start with '>'")
        else 
            let terminalSplits = T.split (=='>') $ T.pack fileContents 
                pairData =  getRawDataPairsFastA modifier (tail terminalSplits) fileName
                (hasDupTerminals, dupList) = checkDuplicatedTerminals pairData
            in
            -- tail because initial split will an empty text
            if hasDupTerminals then errorWithoutStackTrace ("\tInput file " ++ fileName ++ " has duplicate terminals: " ++ show dupList)
            else pairData 
        
-- | getLocusNames extracts locus names after '$' and cretes list of unique
getLocusNames :: [NameText] -> [NameText] -> [NameText]
getLocusNames inList uniqueList =
    if null inList then uniqueList
    else 
        let firstName = T.tail $ T.dropWhile (/= '$') $ head inList
        in
        if firstName `elem` uniqueList then getLocusNames (tail inList) uniqueList
        else getLocusNames (tail inList) (firstName : uniqueList)

-- | getLocusData takes TermData and pull them by locus name--not efficient O(n^2)
-- but only run it once (cold make parallel)
-- locus name is deleted
getLocusData :: [TermData] -> NameText -> [TermData]
getLocusData inDataList locusName =
    if null inDataList then []
    else 
        let firstName = fst $ head inDataList
            firstData = snd $ head inDataList
            taxonName = T.takeWhile (/= '$') firstName
            firstLocus = T.tail $ T.dropWhile (/= '$') firstName
        in
        if firstLocus == locusName then (taxonName, firstData) : getLocusData (tail inDataList) locusName
        else getLocusData (tail inDataList) locusName

-- | outputFastaFiles takes locus name and data and outputs file by locus name.fasta
outputFastaFile :: (NameText, [TermData]) -> IO ()
outputFastaFile (locusName, allData) =
    let fileName = (T.unpack locusName) ++ ".fasta"
        fileString = concat $ fmap getFileString allData
    in do
    writeFile fileName fileString

-- | getFileString creates a single string from TermData
getFileString :: TermData -> String
getFileString (taxName, taxData) =
    let taxLine = ('>' : T.unpack taxName) ++ "\n"
        dataLine = (ST.toString taxData) ++ "\n"
    in 
    if (ST.toString taxData) == "SEQUENCEUNAVAILABLE" then ""
    else taxLine ++ dataLine

-- | main driver
main :: IO ()
main =
  do
    let splash = "\nFastaYX2XY version " ++ fYX2XYVersion ++ "\nCopyright(C) 2021 Ward Wheeler and The American Museum of Natural History\n"
    let splash2 = "nFastaYX2XY comes with ABSOLUTELY NO WARRANTY; This is free software, and may be \nredistributed "
    let splash3 = "under the 3-Clause BSD License.\n"
    hPutStrLn stderr (splash ++ splash2 ++ splash3)
    
    -- Process arguments--a single file containing commands
    args <- getArgs

    if not (length args > 1) then errorWithoutStackTrace "\nProgram requires at least two input files.\n\n"
    else hPutStr stderr "\nInput files: "
    mapM_ (hPutStrLn stderr) args

    fileContentsList <- mapM readFile args

    let initialFastaData = concat $ fmap (getFastA "prealigned") $ zip fileContentsList args

    let locusNames = getLocusNames (fmap fst initialFastaData) []

    hPutStrLn stderr ("There are " ++ (show $ length locusNames) ++ " unique loci")    
    hPutStrLn stderr $ show locusNames

    let dataByLocus = fmap (getLocusData initialFastaData) locusNames

    hPutStrLn stderr ("There are " ++ (show $ fmap length dataByLocus) ++ " taxa per locus") 

    mapM_ outputFastaFile (zip locusNames dataByLocus)   
    
    hPutStrLn stderr "Done"

