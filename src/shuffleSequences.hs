{- |
Module      :  shuffle Sequences
Description :  creates randomized sequences by shuffling elements
               of existing sequences, preserving distributional biases present in source data
Copyright   :  (c) 2024 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

{- 
-}
module Main where

import System.IO
import System.Process
import System.Environment
import Debug.Trace
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import System.Random
import Text.Read (readMaybe)
import qualified Data.Vector as V
import Debug.Trace

-- | 'trim' trim removes leading and trailing white space
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

editorCrap = ['\r']
whiteSpace = [' ', '\t', '\v']

{- getSymbols gets single or multiple characters elements from line
-}
getSymbols :: String -> String -> [String]
getSymbols elementType lineString =
    if elementType == "fastc" then words lineString
    else if elementType == "fasta" then 
                fmap (:[]) $ filter (/= ' ') lineString
    else errorWithoutStackTrace ("Sequence type must be 'fasta' or 'fastc:" <>  elementType) 

{- makeMisssingData takes a random generator, missing float val (0-1), and a fasta formated sequence with name
    and makes data missing (removes from list)
-}
makeMisssingData :: StdGen -> Double -> [String] -> [String]
makeMisssingData inGen missingVal inFastList =
    if null inFastList then []
    else 
        let (randVal, newGen) = uniformR  (0.0 :: Double, 1.0 :: Double) inGen
        in
        if randVal <= missingVal then makeMisssingData newGen missingVal (tail inFastList)
        else (head inFastList) : makeMisssingData newGen missingVal (tail inFastList)

        

-- | formatSequence takes lists and creates fasta or fastc depending on alphabet
formatSequence :: String -> (String, [String]) -> String
formatSequence elementLength (seqName, inL) =
    if null inL then []
    else
        -- fasta charcters of single symbol
        if elementLength == "fasta" then
            ">" <> seqName <> "\n" <> (concat inL) <> "\n"

        -- multicharcter elements
        else 
            ">" <> seqName <> "\n" <> (concat $ fmap (<> " ") inL) <> "\n"

-- | generateShuffledSequences  generates numSeqs shuffled sequences based on inputs
generateShuffledSequences :: StdGen -> String -> Int -> V.Vector (V.Vector String) -> Int -> [[String]] -> [[String]]
generateShuffledSequences inGen fileType numSeqs sequenceLines counter shuffList =
    if counter == numSeqs then shuffList
    else
        let (newGen, firstShuff) = makeShuffledSeq inGen sequenceLines []
        in 
        --trace ("GSS: " <> (show counter) <> " " <> (concat firstShuff)) $
        generateShuffledSequences newGen fileType numSeqs sequenceLines (counter + 1) (firstShuff : shuffList)

-- | makeShuffledSeq makes a single shuffled sequence from inputs
makeShuffledSeq :: StdGen -> V.Vector (V.Vector String) -> [String] -> (StdGen, [String])
makeShuffledSeq inGen sequenceLines curSymbolString =
    let (index, newGen) = randomR  (0, (V.length sequenceLines) - 1) inGen
        termLength = V.length (sequenceLines V.! index)
    in
    -- if random sequence >= length of randomly chosen terminal then return current 
        -- this maintains distribution of lengths as same as input 
    if termLength  <= length curSymbolString then (newGen, reverse curSymbolString)
    else 
        -- get counter-th element of random string
        let newSymbol = (sequenceLines V.! index) V.! (length curSymbolString)
        in
        makeShuffledSeq newGen sequenceLines (newSymbol : curSymbolString) 

-- | getSequences take fasta lines and splits into sequences and returns list 
-- of name, sequence pairs
getSequences :: [String] -> [(String, String)]
getSequences inLineList =
    if null inLineList then []
    else 
        let firstLine = head inLineList
        in
        if null firstLine then getSequences (tail inLineList)
        else if head firstLine == '>' then 
            let (sequenceBody, remainder) = getFirstSequence "" (tail inLineList) (tail inLineList)
            in
            (firstLine, sequenceBody) : getSequences remainder
        else errorWithoutStackTrace ("Format error in input file at line :" <> firstLine)

-- | getFirstSequence removes the first sequnce bnody (before '>') and retuns constenated suquence
-- and reminader of list
getFirstSequence :: String -> [String] -> [String] -> (String, [String])
getFirstSequence curSeqBody remainderStringList inStringList =
    if null inStringList then (curSeqBody, remainderStringList)
    else 
        let firstLine = head inStringList
        in
        if null firstLine then getFirstSequence curSeqBody (tail remainderStringList) (tail inStringList)
        else if head firstLine == '>' then (curSeqBody, remainderStringList)
        else 
            getFirstSequence (curSeqBody <> firstLine) (tail remainderStringList) (tail inStringList) 


-- | 'main' Main Function to run latex IPA csv parser
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        if (length args /= 4) 
            then errorWithoutStackTrace "Require four arguments:\n\tSingle input file (fasta/c),\n\t'fasta' or 'fastc' for single or multiple character element formats,\n\tnumber (Integer) for number of randomized sequences,\n\tand missing fraction (float [0-1.0))" 
            else hPutStrLn stderr "Input args: "
        mapM_ (hPutStrLn stderr) (fmap ('\t':) args)
        --hPutStrLn stderr "\n"

        soundFileHandle <- openFile (head args) ReadMode
        soundContents <- hGetContents soundFileHandle

        let fileType = args !! 1
        let numSeqsMaybe = readMaybe (args !! 2) :: Maybe Int
        if isNothing numSeqsMaybe then errorWithoutStackTrace ("Number of reandomized sequences must be an integer: " <> (args !! 2))
        else hPutStrLn stderr ("Outputing " <> (show $ fromJust numSeqsMaybe) <> " shuffled sequences to stdout")
        let numSeqs = fromJust numSeqsMaybe

        let missingValMaybe = readMaybe (args !! 3) :: Maybe Double
        if isNothing missingValMaybe then errorWithoutStackTrace ("Missing fraction must be a floating point value [0.0-1.0): " <> (args !! 3))
        else 
            if (fromJust missingValMaybe) >= 1.0 || (fromJust missingValMaybe) < 0.0 then errorWithoutStackTrace ("Missing fraction must be a floating point value [0.0-1.0): " <> (args !! 3))
            else hPutStrLn stderr ("Missing fraction " <> (show $ fromJust missingValMaybe) <> " of shuffled sequences")
        let missingVal = fromJust missingValMaybe
        
        --hPutStrLn stderr ("Parsing source file as " <> fileType)
        let fastaSequences = getSequences $ lines soundContents

        let inSequenceLines = fmap snd fastaSequences

        --let inSequenceLines = filter (('>' /=) . head) $ filter (not . null) $ lines soundContents

        let sequenceSymbolLines = V.fromList $ fmap V.fromList $ fmap (getSymbols fileType) inSequenceLines

        --hPutStrLn stderr (show sequenceSymbolLines)

        -- random initialization
        randomGen <- initStdGen

        -- generate random sequences
        let randSeqList = generateShuffledSequences randomGen fileType numSeqs sequenceSymbolLines 0 []

        -- get names of shuffled sequences
        let nameList = zipWith (<>) (replicate numSeqs "randSeq") (fmap show [0..(numSeqs - 1)])
        let fastList = fmap (formatSequence fileType) (zip nameList randSeqList)

        --hPutStrLn stderr (concat fastList)

        -- new random initialization
        randomGen2 <- newStdGen
        -- converts sequences to missing given ,missingVal
        let newData = if missingVal > 0.0 then makeMisssingData randomGen2 missingVal fastList
                      else fastList

        hPutStrLn stdout (concat newData)