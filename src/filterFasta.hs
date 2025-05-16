{- |
Module      :  filterTaaxFromFasta.hs  
Description :  Progam to filter taxa from fasta/c files
Copyright   :  (c) 2025 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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
import Data.List.Split
import Data.Maybe
import Data.Char
import ReadFiles

-- | 'trim' trim removes leading and trailing white space
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace


-- | rawData2Fast converts [(String, [String])] to fast format
rawData2Fast :: [(String, [String])] -> String
rawData2Fast inDataList =
  if null inDataList then []
  else 
    let (firstName, firstData) = head inDataList
        seqData = trim $ (concat $ fmap (++ " ") firstData) ++ "\n"
    in
    ('>':(firstName ++ "\n")) ++ seqData ++ (rawData2Fast $ tail inDataList)


-- | filterNames filters taxa in name list from fasta/c file
filterNames :: [String] -> String -> [(String, [String])] -> [(String, [String])]
filterNames filterList operation inData =
  if null filterList then inData
  else
    if null inData then []
    else 
      let (firstName', firstData') = head inData
          firstName = trim firstName'
          firstData = fmap trim firstData'
      in

      if firstName `elem` filterList then 
          if operation == "include"
            then (firstName, firstData) : filterNames filterList operation (tail inData)
          else filterNames filterList operation (tail inData)
      else 
        if operation == "include" then
          filterNames filterList operation (tail inData)
        else (firstName, firstData) : filterNames filterList operation (tail inData)


-- | main driver
main :: IO ()
main = 
  do 
      args <- getArgs
      if (length args /= 3) 
        then errorWithoutStackTrace "Requires three arguments: input fasta file, a file of taxon names, and 'include' or 'exclude' for filter operation."
        else hPutStrLn stderr "Inputs: "
      mapM_ (hPutStrLn stderr) $ fmap ('\t' :) args
      hPutStrLn stderr ""
      
      fastaFileHandle <- openFile (args !! 0) ReadMode
      fastaFile <-  hGetContents fastaFileHandle
      filterFileHandle <- openFile (args !! 1) ReadMode
      filterFile <- hGetContents filterFileHandle
      let filterList = fmap trim $ words filterFile

      hPutStr stderr $ "Filtering with " <> (args !! 2)
      mapM_ (hPutStr stderr) $ fmap (++ " ") filterList
      hPutStr stderr "\n"

      let operation = if head (args !! 2) `elem` ['i','I'] then "include"
                      else if head (args !! 2) `elem` ['e','E'] then "exclude"
                      else error $ "Third arument must be 'include' or 'exclude': " <>  (args !! 2) 
       
      let (leafDataList, _) = processFastaInput fastaFile

      let newleafData = filterNames filterList operation leafDataList

      let reformatedData = rawData2Fast newleafData

      hPutStr stdout reformatedData

      hPutStrLn stderr "Done"
      
       
