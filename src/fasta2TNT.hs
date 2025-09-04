{- |
Module      :  fasta2TNT.hs 
Description :  Progam to covnvert aligned sequence fasta files to TNT format with and w/o indels
Copyright   :  (c) 2019 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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



-- | recodeNucleotide takes a single letter nucleotide charcater and returns the numerical equivalent
-- indels as missing or 5th state
recodeNucleotide :: String -> Char -> String 
recodeNucleotide indelMissing inChar =
  if inChar == ' ' then ""
  else if inChar == '-' then
    if indelMissing == "?" then "?"
    else if indelMissing == "5" then "5"
    else errorWithoutStackTrace ("Unrecognized option for indel coding: " ++ indelMissing)
  else -- non-indel nucleotides
    let recChar = toUpper inChar
    in
    if recChar == 'A' then "0"
    else if recChar == 'C' then "1"
    else if recChar == 'G' then "2"
    else if recChar == 'T' then "3"
    else if recChar == 'U' then "3"
    else if recChar == 'X' then "[0123]"
    else if recChar == 'Y' then "[13]"
    else if recChar == 'R' then "[02]"
    else if recChar == 'S' then "[12]"
    else if recChar == 'W' then "[04]"
    else if recChar == 'M' then "[01]"
    else if recChar == 'K' then "[23]"
    else if recChar == 'B' then "[123]"
    else if recChar == 'D' then "[023]"
    else if recChar == 'H' then "[013]"
    else if recChar == 'V' then "[012]"
    else if recChar == 'N' then "[0123]"
    else if recChar == '?' then "?"
    else if recChar == '*' then "?"
    else errorWithoutStackTrace ("Unrecognized nucleotide: " ++ [inChar])
     
-- | recodeLine recode and printys fasta lines 
-- if '>' then printys name otherwise recodes nucleotiedes
-- checks length all match as well
recodeLine :: Handle -> Int -> String -> String -> IO ()
recodeLine whereTo numCharacters indelMissing inLine =
  if null inLine then errorWithoutStackTrace "Empty line in recodeLine"
  else if  numCharacters == 0 then errorWithoutStackTrace "Zero characters to recode"
  else
    if head inLine == '>' then 
      do hPutStr whereTo ((head $ words $ tail inLine) ++ " ")
    else 
      let outString = concat $ fmap (recodeNucleotide indelMissing) inLine
      in
      if length inLine /= numCharacters then errorWithoutStackTrace ("Number of charcaters varies " ++ show numCharacters ++ " v " ++ (show $ length outString))
      else 
        do hPutStrLn whereTo outString


-- | main driver
main :: IO ()
main = 
  do 
       args <- getArgs
       if (length args /= 2) 
          then errorWithoutStackTrace "Need 2 arguments: Indels as missing or 5th state (?/5) and input fasta file"
          else hPutStrLn stderr ("Openning fasta file " ++ (args !! 1))
       let inDelType = head args
       fastaFileHandle <- openFile (args !! 1) ReadMode
       fastaFile <-  hGetContents fastaFileHandle
       let fastaLines = filter (/= []) $ lines fastaFile
       if (mod (length fastaLines)  2) > 0 
          then errorWithoutStackTrace "Odd number of input lines should be two per taxon"
          else do 
            let numTaxa = quot (length fastaLines) 2
            let numChars =  length (fastaLines !! 1)
            hPutStr stdout ("xread ")
            hPutStr stdout ("\'Data recoded from " ++ (args !! 1) ++ " with indels coded as " ++ inDelType  ++ "\' ")
            hPutStrLn stdout (show numChars ++ " " ++ show numTaxa)
            mapM_ (recodeLine stdout numChars inDelType) fastaLines
            hPutStrLn stdout ";\ncc -.;\nproc /;\n"
            hPutStrLn stderr (show numTaxa ++ " taxa recoded with " ++ show numChars ++ " characters")
       
