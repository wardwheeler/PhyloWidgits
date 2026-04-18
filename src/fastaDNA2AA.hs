{- |
Module      :  fastaDNA2AA.hs 
Description :  Progam to converts DNA fasta files to Amino acid codes via "standard" triplets
Copyright   :  (c) 2026 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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


-- codon2AA tabel for coversion
codon2AA :: [Char] -> Char
codon2AA x 
    | x `elem` ["GCT", "GCC", "GCA", "GCG"] = 'A'
    | x `elem` ["CGT", "CGC", "CGA", "CGG", "AGA", "AGG"] = 'R'
    | x `elem` ["AAT", "AAC"]                             = 'N'
    | x `elem` ["GAT", "GAC"]                             = 'D'
    | x `elem` ["TGT", "TGC"]                             = 'C'
    | x `elem` ["CAA", "CAG"]                             = 'Q'
    | x `elem` ["GAA", "GAG"]                             = 'E'
    | x `elem` ["GGT", "GGC", "GGA", "GGG"]               = 'G'
    | x `elem` ["CAT", "CAC"]                             = 'H'
    | x `elem` ["ATT", "ATC", "ATA"]                      = 'I'
    | x `elem` ["ATG"]                                    = 'M'
    | x `elem` ["TTA", "TTG", "CTT", "CTC", "CTA", "CTG"] = 'L'
    | x `elem` ["AAA", "AAG"]                             = 'K'
    | x `elem` ["TTT", "TTC"]                             = 'F'
    | x `elem` ["CCT", "CCC", "CCA", "CCG"]               = 'P'
    | x `elem` ["TCT", "TCC", "TCA", "TCG", "AGT", "AGC"] = 'S'
    | x `elem` ["ACT", "ACC", "ACA", "ACG"]               = 'T'
    | x `elem` ["TGG"]                                    = 'W'
    | x `elem` ["TAT", "TAC"]                             = 'Y'
    | x `elem` ["GTT", "GTC", "GTA", "GTG"]               = 'V'
    | x `elem` ["TAA", "TGA", "TAG"]                      = '*'
    | x `elem` ["---", "..."]                             = '-'
    | x == "~~~"                                          = '-'
    | "N" `isInfixOf` x                                 = 'X'
    | "-" `isInfixOf` x                                 = '-'
    | "." `isInfixOf` x                                 = '-'
    -- | otherwise = errorWithoutStackTrace ("Unidentified codon: " <> x)
    | otherwise = 'X'

-- getAA takes a triplet and returns Amino acid code via standard table
getAA :: [Char] -> Char
getAA inCodon =
  if length inCodon /= 3 then errorWithoutStackTrace ("Codon 'triplet' does not have 3 nucleorides: " <> inCodon)
  else 
    codon2AA inCodon


-- convert2AminoAcids takes fasta lines and if starts with '>" returns, else
-- assumes in phase and converts to amino acid codes
convert2AminoAcids :: String -> String
convert2AminoAcids inString =
  if null inString then []
  else if head inString == '>' then inString <> "\n"
  else 
    let inThrees = chunksOf 3 inString
    in
    (fmap getAA inThrees) <> "\n"


-- | main driver
main :: IO ()
main = 
  do 
       args <- getArgs
       if (length args /= 1) 
          then errorWithoutStackTrace "Need single argument: input fasta file"
          else hPutStrLn stderr ("Openning fasta file " ++ (args !! 0))
       fastaFileHandle <- openFile (args !! 0) ReadMode
       fastaFile <-  hGetContents fastaFileHandle
       let fastaLines = filter (/= []) $ lines fastaFile
       if (mod (length fastaLines)  2) > 0 
          then errorWithoutStackTrace "Odd number of input lines should be two per taxon"
          else do 
            let aminoLines = concat $ fmap convert2AminoAcids fastaLines
            hPutStrLn stdout aminoLines
       
