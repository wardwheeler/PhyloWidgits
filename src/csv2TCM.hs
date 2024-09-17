{- |
Module      :  csv2tcm
Description :  Converts csv sound file to tcm for fastc language file--requires fastc file as input
Copyright   :  (c) 2016 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

{- Need to add
    1) generate tcm files with alphabets and cost matrices
    2) optinos for alternate cost regimes
-}

module Main where

import System.IO
import System.Process
import System.Environment
import Debug.Trace
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Set as S
import Data.Maybe
import Text.ParserCombinators.Parsec
import Data.CSV
import Data.Either
import qualified Data.Vector as V
import ReadFiles

-- | convert2Vector takes [[String]] and returns vector of vectors
convert2Vector :: [[String]] -> V.Vector (V.Vector String)
convert2Vector inList =
  if null inList then error "Empty contents to convert2Vector"
  else
    let firstVect = V.fromList (tail inList)
    in
    V.map (V.fromList . tail) firstVect

-- | getSounds takes file contents and filters aout names and returns orderred list of unique sounds.
getSounds :: String -> [String]
getSounds inString =
  if null inString then error "Empty sound contents"
  else
      let sounds = removeNames $ words inString
      in
      sort $ nub sounds

-- | removeNames remove strings that bewgin with '>' as in fasta/c files
removeNames :: [String] -> [String]
removeNames inList =
  if null inList then inList
  else
      let candidate = head inList
      in
      if (head candidate) == '>' then removeNames (tail inList)
      else candidate : removeNames (tail inList)

-- | makeCostMatrix takes the sound list and vector of sound aspects and type to produce the cost matrix to be printed for
--the tcm.  THe matrix is square with 1 + length to account for indel costs
makeCostMatrix :: [String] -> V.Vector (V.Vector String) -> String -> [[String]]
makeCostMatrix  soundList aspectMatrix costType =
  if null soundList then error "No sound list" 
  else if V.null aspectMatrix then error "No aspect matrix"
  else if null costType then error "No cost type specified"
  else 
      if costType == "11" then
          makeStringMatrix [[]] "1 " "1 " (length soundList) 0
      else if costType == "12" then
          makeStringMatrix [[]] "1 " "2 " (length soundList) 0
      else if costType == "vcn" then
          makeValueMatrix [[]] (V.fromList soundList) aspectMatrix "2 " costType (length soundList) 0
      else if costType == "vcn2" then
          makeValueMatrix [[]] (V.fromList soundList) aspectMatrix "4 " costType (length soundList) 0
      else if costType == "all5" then
          makeValueMatrix [[]] (V.fromList soundList) aspectMatrix "5 " costType (length soundList) 0
      else error "Unrecognized cost type"

-- | makeValueMatrix creats cost matrix for sounds based on soundlist and aspect matrix
makeValueMatrix :: [[String]] -> V.Vector String -> V.Vector (V.Vector String) -> String -> String -> Int -> Int -> [[String]]
makeValueMatrix inMatrix soundVector aspectMatrix indelCost costType size row = 
  if row == size then (inMatrix ++ [(makeRowValue [] soundVector aspectMatrix indelCost costType size row 0)])
  else 
    (inMatrix ++ [(makeRowValue [] soundVector aspectMatrix indelCost costType size row 0)] ++ (makeValueMatrix inMatrix soundVector aspectMatrix indelCost costType size (row + 1)))

-- | makeRowValue takes sound list and aspects to create row of cost matrix
makeRowValue :: [String] -> V.Vector String -> V.Vector (V.Vector String) -> String -> String -> Int -> Int -> Int -> [String]
makeRowValue inRow soundVector aspectMatrix indelCost costType size row column =
  if (row == size) && (column == size) then (inRow ++ ["0\n"])
  else if column == size then (inRow ++ [indelCost] ++ ["\n"])
  else if row == size then 
          (inRow ++ [indelCost] ++ (makeRowValue inRow soundVector aspectMatrix indelCost costType size row (column + 1)))
  else if row == column then
          (inRow ++ ["0 "] ++ (makeRowValue inRow soundVector aspectMatrix indelCost costType size row (column + 1)))
  else 
          let elementCost = getAspectCost soundVector aspectMatrix costType row column in
          (inRow ++ [(show elementCost) ++ " "] ++ (makeRowValue inRow soundVector aspectMatrix indelCost costType size row (column + 1)))

-- | findRow takes sound and aspect matrix and return index of sound row
findRow :: Int -> String -> V.Vector (V.Vector String) -> Int
findRow index soundString aspectMatrix = 
  if index == V.length aspectMatrix then error ("Sound " ++ soundString ++ " not found")
  else
      if soundString == V.head (aspectMatrix V.! index) then index
      else findRow (index + 1) soundString aspectMatrix

-- | getAspectCost takes sound list, aspects, indices and cost tyhpe to return edit cost
getAspectCost :: V.Vector String -> V.Vector (V.Vector String) -> String -> Int -> Int -> Int
getAspectCost soundVector aspectMatrix costType row column = 
  let rowIndex = findRow 0 (soundVector V.! row) aspectMatrix
      columnIndex = findRow 0 (soundVector V.! column) aspectMatrix
  in    
  if (costType == "vcn") || (costType == "vcn2") then 
    if (null ((aspectMatrix V.! rowIndex) V.! 1)) || (null ((aspectMatrix V.! columnIndex) V.! 1)) then 1
    else if ((aspectMatrix V.! rowIndex) V.! 1) == ((aspectMatrix V.! columnIndex) V.! 1)  then 1
    else 2
  else if costType == "all5" then 
      countDiffs (aspectMatrix V.! rowIndex) (aspectMatrix V.! columnIndex) 0 5 0
  else error "Unrecognized cost type"

-- | countDiffs takes 2 vetors of aspects amd counts differneces in first 5
countDiffs :: V.Vector String -> V.Vector String -> Int -> Int -> Int -> Int
countDiffs rowVect columnVect index maxIndex cost =
  if index == maxIndex then cost
  else 
      if (rowVect V.! index) == (columnVect V.! index) then 
        countDiffs rowVect columnVect (index + 1) maxIndex cost
      else if (null (rowVect V.! index)) || (null (columnVect V.! index)) then
        countDiffs rowVect columnVect (index + 1) maxIndex cost
      else 
        countDiffs rowVect columnVect (index + 1) maxIndex (cost + 1)

-- | makeStringMatrix takes twoString values and a pair of indices and num roww/columns and returns
-- a list of list of strings with first value on non-diagonals and second in last column and row
makeStringMatrix :: [[String]] -> String -> String -> Int -> Int -> [[String]]
makeStringMatrix inMatrix nonDiag lastRowColumn size row =
  if row == size then (inMatrix ++ [(makeRow [] nonDiag lastRowColumn size row 0)])
  else 
      (inMatrix ++ [(makeRow [] nonDiag lastRowColumn size row 0)] ++ (makeStringMatrix inMatrix nonDiag lastRowColumn size (row + 1)))

-- | makeRow takes non-diag and final strings, two indicews, and size and make a row for makeCostMatrix
makeRow :: [String] -> String -> String -> Int -> Int -> Int -> [String]
makeRow inRow nonDiag lastRowColumn size row column =
  if (row == size) && (column == size) then (inRow ++ ["0\n"])
  else if column == size then (inRow ++ [lastRowColumn] ++ ["\n"])
  else if row == size then 
          (inRow ++ [lastRowColumn] ++ (makeRow inRow nonDiag lastRowColumn size row (column + 1)))
  else if row == column then
          (inRow ++ ["0 "] ++ (makeRow inRow nonDiag lastRowColumn size row (column + 1)))
  else
          (inRow ++ [nonDiag] ++ (makeRow inRow nonDiag lastRowColumn size row (column + 1)))

-- | 'main' Main Function to run latex IPA csv parser
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        if (length args /= 3) 
          then error "Two input files required (word sound attribute csv and fastc) and a tcm type"
          else hPutStrLn stderr "Inputs: "
        mapM_ (hPutStrLn stderr) args
        --hPutStrLn stderr ""
        --soundFileHandle <- openFile (head args) ReadMode
        --result <- parseFromFile csvFile (head args)
        csvResult <- parseFromFile csvFile (head args)
        let soundAspects = case csvResult of
                          Left err -> error $ "Error parsing " ++ show (head args) ++ " " ++ show err 
                          Right result -> result
        fastcFileHandle <- openFile (args !! 1) ReadMode
        fastcContents <- hGetContents fastcFileHandle
        let tcmType =  (last args)
        let tcmFileName = (args !! 1) ++ "." ++ (last args) ++ ".tcm"
        hPutStrLn stderr ("Creating tcm file " ++ tcmFileName)
        let vectSoundAspects = convert2Vector soundAspects
        let soundList = getSounds fastcContents
        let editMatrix = makeCostMatrix soundList vectSoundAspects tcmType 
        hPutStrLn stderr ("There are " ++ show (length soundList) ++ " sounds in " ++ (args !! 1))
        hPutStrLn stderr (show soundList)
        tcmFileHandle <- openFile tcmFileName WriteMode
        let soundListSpaces = fmap (++ " ") soundList
        mapM_ (hPutStr tcmFileHandle) soundListSpaces
        hPutStrLn tcmFileHandle ""
        mapM_ (hPutStr tcmFileHandle) (concat editMatrix)
        hClose tcmFileHandle
        --hPutStr stdout (show soundAspects)
        

