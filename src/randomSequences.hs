{- |
Module      :  randomSequences
Description :  generates random seqeunces according to alphabet, average and variation length, and missing fraction
               missing keeps names so can be combined across multiple instances.
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

{- THeera re a set of ccontrol charcters that can't be in ipa element symbols set
['{','}',':','"',';'] that are converted to to symbols not in ipa or phyloAlgInfo grammar
and can converted back
new set ['<','>','#','&']
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


-- | 'trim' trim removes leading and trailing white space
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

editorCrap = ['\r']
whiteSpace = [' ', '\t', '\v']

{- getSymbols gets single or multiple characters elements from line
tries to filter out trash
-}
getSymbols :: String -> String -> [String]
getSymbols elementType lineString' =
    let lineString = filter isPrint lineString'
    in
    if elementType == "fastc" then words lineString
    else if elementType == "fasta" then 
                fmap (:[]) $ filter (/= ' ') lineString
    else errorWithoutStackTrace ("Sequence type must be 'fasta' or 'fastc:" <>  elementType) 


{- processArgs takes strings of arguments and returns parameters for seqeunce generation
-}
processArgs :: [String] -> (String, Int, Int, Int, Double)
processArgs inArgs =
    if null inArgs then error "Empty program arguments"
    else 
        let alphabetFileName = fst $ getParam "alphabet" inArgs
            aveLength =  snd $ getParam "avelen" (fmap (fmap toLower) inArgs)
            varLength =  snd $ getParam "varlen" (fmap (fmap toLower) inArgs) 
            numSeqs = snd $ getParam "numseqs" (fmap (fmap toLower) inArgs) 
            missing = snd $ getParam "missing" (fmap (fmap toLower) inArgs)
        in (alphabetFileName, aveLength, varLength, numSeqs, fromIntegral missing / 10000.0)

-- | getParam retunrs alphabet file name from arg list
getParam :: String -> [String] -> (String, Int)
getParam target inList =
    if null inList then errorWithoutStackTrace (target <> " parameter not found in arg list (required: alphabet, avelen, varlen, numseqs, missing) in format 'paramter:value'")
    else 
        let first = head inList
            firstPart = takeWhile (/=':') first
            secondPart = tail $ dropWhile (/=':') first
        in
        --trace ("GP: " <> first <> " " <> firstPart <> " " <> secondPart) $
        if (fmap toLower) firstPart == target then 
                if target == "alphabet" then (secondPart, 0)
                else if target == "missing" then
                    let value = readMaybe secondPart :: Maybe Double
                    in
                    if isNothing value then errorWithoutStackTrace (secondPart <> " in " <> first <> "is not a float [0.0,1.0)")
                    else if fromJust value < 0.0 || fromJust value >= 1.0 then errorWithoutStackTrace ("\nError " <> first <> "' is out of range [0.0,1.0) : " <> (show $ fromJust value))
                    else ("", floor $ 10000.0 * (fromJust value))
                else 
                    let value = readMaybe secondPart :: Maybe Int
                    in
                    if isNothing value then errorWithoutStackTrace (secondPart <> " in " <> first <> "is not an integer")
                    else ("", fromJust value)
        else getParam target $ tail inList

-- | generateRandomSequence 
generateRandomSequence :: Int -> V.Vector String -> (Int, Int) -> [String]
generateRandomSequence counter alphabetVect (inGen, length) = 
    if counter == length then []
    else 
        let (index, newGen) = randomR  (0, (V.length alphabetVect) - 1) (mkStdGen inGen)
            (newIntGen, _) = randomR  (0, maxBound :: Int) newGen
        in
        (alphabetVect V.! index) : generateRandomSequence (counter + 1) alphabetVect (newIntGen, length)

-- | generateLengthList gernartee a klist fomr lengths uniformly at random fomr min and max
generateLengthList :: StdGen -> Int -> Int -> Int -> Int -> [Int]
generateLengthList inGen minLength maxLength numVals counter  = 
    if counter == numVals then []
    else 
        let (index, newGen) = randomR  (minLength, maxLength) inGen
        in
        index : generateLengthList newGen minLength maxLength numVals (counter + 1)

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
formatSequence :: Int -> (String, [String]) -> String
formatSequence elementLength (seqName, inL) =
    if null inL then []
    else
        -- fasta charcters of single symbol
        if elementLength == 1 then
            ">" <> seqName <> "\n" <> (concat inL) <> "\n"

        -- multicharcter elements
        else 
            ">" <> seqName <> "\n" <> (concat $ fmap (<> " ") inL) <> "\n"

-- | 'main' Main Function to run latex IPA csv parser
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        if (length args /= 5) 
            then errorWithoutStackTrace "Require five arguments alphabet:fileName avelen:N varlen:N numseqs:N missing:N (0.0-1.0]" 
            else hPutStr stderr "Arguments:\n"
        mapM_ (hPutStrLn stderr) (fmap ('\t':) args)
        hPutStrLn stderr "\n"

        let (alphabetFile, aveLength, varLength, numSeqs, missingVal) = processArgs args
    
        -- parse alphabet file
        alphabetFileHandle <- openFile alphabetFile ReadMode
        alphabetContents <- hGetContents alphabetFileHandle

        -- this is a fix for Uto-Aztecan might a garbage character encoding residue
        let alphabetElements = words alphabetContents

        -- for fasta/c format 
        let maxAlphLength = maximum $ fmap length alphabetElements


        hPutStrLn stderr $ "Parameters " <> (show (alphabetElements, aveLength, varLength, numSeqs, missingVal))

        let maxLength = aveLength + (ceiling (fromIntegral varLength / 2.0))
        let minLength = aveLength - (ceiling (fromIntegral varLength / 2.0))

        -- random seed generation 
        randomGen1 <- initStdGen

        -- length of sequences
        let seqLengthList = generateLengthList randomGen1 minLength maxLength numSeqs 0


        -- new random initialization
        randomGen2 <- newStdGen

        -- generate random sequences
        let randGenList = generateLengthList randomGen2 0 (maxBound :: Int) numSeqs 0

        let randSeqList = fmap (generateRandomSequence 0 (V.fromList alphabetElements)) (zip randGenList seqLengthList)
        
        
        -- generate names for sequnces
        let nameList = zipWith (<>) (replicate numSeqs "randSeq") (fmap show [0..(numSeqs - 1)])
        let fastList = fmap (formatSequence maxAlphLength) (zip nameList randSeqList)
        
        
        -- new random initialization
        randomGen3 <- newStdGen
        -- converts sequences to missing given ,missingVal
        let newData = if missingVal > 0.0 then makeMisssingData randomGen3 missingVal fastList
                      else fastList

        hPutStrLn stdout $ concat newData
        