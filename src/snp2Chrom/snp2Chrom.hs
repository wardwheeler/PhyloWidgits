{- |
Module      :  snp2Chrom
Description :  inputs VCF file of individual genome SNP vaiants and removes extraneous info
               outputting individual files for each chromosome with 4 tuple of
               information (Chromosome, Position, reference genome state, Variant state)
               BUT uses lazy text to help with memory consumption
Copyright   :  (c) 2019-2021 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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
import System.Environment
--import Data.Maybe

-- | baseRecode takes base calls and recodes as single letter IUPAC
baseRecode :: String -> String
baseRecode inString 
    | null inString = error "Null input in baseRecode"
    | length inString == 1 = inString
    | inString == "A,C" = "M"
    | inString == "A,G" = "R"
    | inString == "A,T" = "W"
    | inString == "C,G" = "S"
    | inString == "C,T" = "Y"
    | inString == "G,T" = "K"
    | inString == "A,C,G" = "V"
    | inString == "A,C,T" = "H"
    | inString == "A,G,T" = "D"
    | inString == "C,G,T" = "B"
    | inString == "A,C,G,T" = "N"
    | inString == "AC" = "M"
    | inString == "CA" = "M"
    | inString == "AG" = "R"
    | inString == "AT" = "W"
    | inString == "CG" = "S"
    | inString == "CCT" = "Y"
    | inString == "CT" = "Y"
    | inString == "TC" = "Y"
    | inString == "GT" = "K"
    | inString == "ACG" = "V"
    | inString == "ACT" = "H"
    | inString == "AGT" = "D"
    | inString == "CGT" = "B"
    | inString == "ACGT" = "N"
    | otherwise = error ("Unrecognized polymorphism " ++ inString)


-- | filterLines removes lines that start with #
filterLines :: [String] -> [String]
filterLines inLines =
    if null inLines then []
    else 
        if head (head inLines) == '#' then filterLines  (tail inLines)
        else head inLines : filterLines (tail inLines)

-- | get4tuple reads lines from VCF file and extracts the 4 pieces of info:
-- chromosome, position, reference genome state, variant state
get4tuple :: String -> (String, String, String, String)
get4tuple lineString =
    let parts = words lineString
        chromosomeNumber = parts !! 0
        position = parts !! 1
        referenceState = baseRecode (parts !! 3)
        variantState = baseRecode (parts !! 4)
    in
    --trace (show chromosomeNumber ++ " " ++ show position ++ " " ++ show referenceState ++ " " ++ show vaiantState) 
    (chromosomeNumber, position, referenceState, variantState)

-- | splitOnChrom splits the tuples list by the first element which is the chromosome 
-- identifier 1-22, X, Y, mt
splitOnChrom :: [(String, String, String, String)] -> [[(String, String, String, String)]] -> [[(String, String, String, String)]]
splitOnChrom inTupleList curList =
   if null inTupleList then curList
    else 
        let (curChrom, _, _, _) = head inTupleList
            thisList = getChromList inTupleList curChrom
            newList = curList ++ [thisList]
        in
        splitOnChrom (drop (length thisList) inTupleList) newList

-- | getChromList make a list of all tuples untill chromosome ID changes
getChromList ::  [(String, String, String, String)] -> String -> [(String, String, String, String)]
getChromList inTupleList prevChrom =
    if null inTupleList then []
    else 
        let (curChrom, _, _, _) = head inTupleList
        in
        --trace (prevChrom ++ " " ++ curChrom ++ " " ++ show (length inTupleList) ++ " " ++ pos) (
        if curChrom /= prevChrom then []
        else (head inTupleList) : getChromList (tail inTupleList) curChrom
        

-- |  fourTuple2String converts four-tuple to string for output
fourTuple2String :: (String, String, String, String) -> String
fourTuple2String (a,b,c,d) = a ++ " " ++ b ++ " " ++ c ++ " " ++ d 

-- | printChrom take a strub and chromosome variant list and prints to file
-- with name stub ++ chromID ++.SNP
printChrom :: String -> [(String, String, String, String)] -> IO ()
printChrom stub variantList =
    let (chromID, _,_,_) = head variantList
        fileName = stub ++ "." ++ chromID ++ ".SNP"
        stringList = fmap fourTuple2String variantList
    in
    do
        outFileHandle <- openFile fileName WriteMode
        mapM_ (hPutStrLn outFileHandle) stringList
        hClose outFileHandle


-- | 'main' Main Function 
main :: IO ()
main = 
    do
        --get input file name for outputs, stub name for outputs 
        args <- getArgs
        if (length args < 1) then error "At least on arg required: input VCF file.  A second arg with stub string is optional"
        else hPutStrLn stderr ("Input VCF file name: " ++ (head args))
        if (length args == 2) then hPutStrLn stderr ("File output stub name: " ++ (last args))
        else hPutStrLn stderr ("File output stub name: " ++ (takeWhile (/= '.') (head args)))
        inFileHandle <- openFile (args !! 0) ReadMode
        inContents <- hGetContents inFileHandle
        let inLines = lines inContents
        let newLines = filterLines inLines
        let guts = fmap get4tuple newLines
        let gutsChromList = splitOnChrom guts []
        --mapM_ (hPutStr stderr) $ fmap show guts
        --hPutStrLn stderr ("There were " ++ show (length guts) ++ " SNP variants in " ++ show (length gutsChromList) ++ " chromosomes")
        if (length args == 2) then mapM_ (printChrom (last args)) gutsChromList 
        else mapM_ (printChrom (takeWhile (/= '.') (head args))) gutsChromList 
        hPutStrLn stderr "All done"


