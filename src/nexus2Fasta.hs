{- |
Module      :  nexus2Fasta
Description :  Converts nexus to fasta file
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
import Data.List
import Debug.Trace

-- | printFasta takes list of two strings (name and sequence) and 
-- prints with '>' etc
printFasta :: [String] -> IO ()
printFasta stuff =
    if null stuff then putStr ""
    else 
        let curStuff = words $ head stuff
        in
        if length curStuff < 2 then 
            do
                putStr ""
        else
            do
                putStr (">" ++ (head curStuff) ++ "\n" ++ (last curStuff) ++ "\n")
                printFasta (tail stuff)

-- | subRN subsitutes /n for /r so lines will work
subRN :: String -> String
subRN inString =
    if null inString then []
    else
        let inChar = head inString
        in
        if (inChar == '\r') then '\n' : (subRN (tail inString))
        else inChar : (subRN $ tail inString)

-- | getGuts removes initial stuff through "matrix" line and end stuff after ";"
getGuts :: String -> [String]
getGuts inString =
    let inWords = lines inString
        notStartList = dropWhile (/= "matrix") inWords
        notStartList' = drop 1 notStartList
        middlePart = takeWhile (/= ";") notStartList'
    in middlePart

-- | convertSeq takes a sequnece and replaces leading gaps with ?
convertSeq :: String -> String
convertSeq inString =
    if null inString then []
    else 
        if (head inString) /= '-' then inString
        else '?' : convertSeq (tail inString)

-- | convertLeadingTrailing converts leading and trailing '-' to '?'
convertLeadingTrailing :: String -> String
convertLeadingTrailing inLine = 
    if null inLine then []
    else 
        let seqName = head $ words inLine
            seqVal = last $ words inLine
            leadingConverted = convertSeq seqVal
            trailingConverted =  convertSeq $ reverse leadingConverted
            newSeq = reverse trailingConverted
        in
        seqName <> " " <> newSeq
        


-- | Main function for conversion
main :: IO ()
main = 
  do 
     --get input command filename
    args <- getArgs
    if (length args /= 2) 
      then error "Require (true/false) argument to convert leading trailing gaps to '?' arg and input nexus file name"
      else hPutStrLn stderr "Inputs: "
    mapM_ (hPutStrLn stderr) args
    hPutStrLn stderr ""
    
    let lt2QuesMark = head args 

    if lt2QuesMark `notElem` ["true", "false"] 
        then errorWithoutStackTrace ("Arg must be 'true' or 'false' argument was: " <> lt2QuesMark)
        else hPutStrLn stderr ("Convert leading trailing gaps to '?' " <> lt2QuesMark)

    nexusFileHandle <- openFile (last args) ReadMode
    
    allFile <-  hGetContents nexusFileHandle
    hPutStrLn stderr $ "There are " <> (show $ length $ lines allFile) <> " lines in the input nexus file" 
    -- let allFileProcessed = subRN allFile
    -- let rawGuts = filter (/= '\t') $ filter (/= '\'') allFileProcessed
    let guts = getGuts allFile -- lines $ takeWhile (/= ';') rawGuts -- $ unlines $ drop 8 $ lines rawGuts
    --mapM_ (hPutStrLn stderr) guts
    hPutStrLn stderr $ "There are " <> (show $ length guts) <> " sequences to output"
    let guts' = if lt2QuesMark == "false" then guts
                else fmap convertLeadingTrailing guts
    printFasta guts'
