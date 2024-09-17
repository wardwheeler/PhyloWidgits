{- |
Module      :  Mean Word
Description :  Calculates mean words of languages from fastc files (latex IPA) 
Copyright   :  (c) 2015 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

import System.IO
import System.Process
import System.Environment
import Debug.Trace
import Data.List
import Data.MultiSet (fromList, toOccurList)
import ReadFiles

-- | 'getLangPairs' getLangPairs takes string name of file
-- and returns list of language names and a list of Strings of language
-- symbols
getLangPairs :: String -> IO [(String, [String])]
getLangPairs inName =
    if null inName then error "No file name"
    else
        do
            inFileHandle <- openFile inName ReadMode
            inContents <- hGetContents inFileHandle
            let (rawData, _) = processCustomAlphabetNoMatrix inContents --processFastaInput inContents
            --hClose inFileHandle
            return rawData

-- | 'getCharNUmbers' getCharNumbers takes the list of data pairs (name, char stinrs) and
-- returns the number of each char in the strings paired with the name argument
getCharNumbers :: [(String, [String])] -> String -> (String, [(String, Int)])
getCharNumbers inDataPairs inName =
    if null inDataPairs then error "No data pairs"
    else if null inName then error "No language name to find"
    else 
        ---let newList = filter (inName == fst) inDataPairs
        let newList = [ x | x <- inDataPairs, (fst x == inName)]
            (_, newCharList) = unzip newList
            newChars = sort $ concat $ fmap words $ concat newCharList
            --newChars = sort $ concat newCharList
            charFreq = toOccurList $ fromList newChars
        in
        trace (inName ++ ": " ++ show (length charFreq) ++ " unique sounds\n")
        (inName, charFreq)

-- | 'insertIfZero' checks if character symbol in list and if not inserts zero pair
insertIfZero :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
insertIfZero zeroPairs inPairs =
    if null zeroPairs then inPairs
    else
        let firstZeroPair = head zeroPairs
            firstZero = fst firstZeroPair
            isInPairs = lookup firstZero inPairs
        in
        if isInPairs /= Nothing then  insertIfZero (tail zeroPairs) inPairs
        else insertIfZero (tail zeroPairs) (insert firstZeroPair inPairs)

-- | 'addZeroPairs' addZeroPairs takes a list of (String, 0) pairs and adds them to language frequency
-- if the sound is not in the frequency list--this adds 0 occurences to character lists
addZeroPairs :: [(String, Int)] -> (String, [(String, Int)]) -> (String, [(String, Int)])
addZeroPairs zeroPairs inPair =
    if null zeroPairs then error "No pairs"
    else 
        let (langName, inCharacters) = inPair
            outCharacters = insertIfZero zeroPairs inCharacters
        in
        (langName, outCharacters)

-- | 'getFloatList' getFloatList divides [[Int]] by [Int] to get a normlaized float list
getFloatList :: [Double] -> [[Double]] -> [[Double]]
getFloatList sumList intLists =
    if null sumList then intLists
    else
        let firstSum = head sumList
            firstIntList = head intLists
            floatList = fmap (/firstSum) firstIntList
        in
        trace ("Sum " ++ show firstSum)
        floatList : getFloatList (tail sumList) (tail intLists)


-- | 'main' Main Function to run PCG
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        allStuff <- mapM getLangPairs args
        let allFlat = concat allStuff
        --let langPairs = head allStuff
        let (langNames, langChars) = unzip allFlat
        hPutStrLn stderr (show langChars)
        let allLangNames = nub langNames
        let allLangChars = sort $ nub $ concat $ fmap words $ concat langChars
        let zeroList = replicate (length allLangChars) 0
        let zeroPairs = zip allLangChars zeroList
        let freqList = fmap (getCharNumbers allFlat) allLangNames
        let completeList = fmap (addZeroPairs zeroPairs) freqList
        let (nameList, freqPairList) = unzip completeList
        let freqListList = fmap snd $ fmap unzip freqPairList
        let totalSoundList = fmap sum freqListList
        let floatList = getFloatList (fmap fromIntegral totalSoundList) (fmap (fmap fromIntegral) freqListList)
        let freqList = zip  nameList floatList
        hPutStrLn stderr "Input files: "
        mapM_ (hPutStr stderr) args
        mapM_ (hPutStrLn stderr) allLangNames
        hPutStrLn stderr ("\nTotal sounds: " ++ show (length allLangChars))
        hPutStrLn stdout ("Language, " ++ show allLangChars)
        hPutStrLn stdout (show freqList)
        

