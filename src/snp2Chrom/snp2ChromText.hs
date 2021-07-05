{- |
Module      :  snp2Chrom
Description :  inputs VCF file of individual genome SNP vaiants and removes extraneous info
               outputting individual files for each chromosome with 4 tuple of
               information (Chromosome, Position, reference genome state, Variant state)
               BUT uses lazt T.Text to help with memory consumption
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
import System.Environment
import Data.List
--import Data.Maybe
import qualified Data.Text.Lazy as T


--hasN checks Text for presence of 'N' reports True if found
hasN :: T.Text -> Bool
hasN inText =
    if T.null inText then False
    else 
        let isThere = T.find (== 'N') inText
        in
        if isThere == Nothing then False
        else True

--hasDel checks Text for presence of 'N' reports True if found
hasDel :: T.Text -> Bool
hasDel inText =
    if T.null inText then False
    else 
        let isThere = T.find (== '<') inText
        in
        if isThere == Nothing then False
        else True

-- | baseRecode takes base calls and recodes as single letter IUPAC
-- sort input?
baseRecode :: T.Text -> T.Text
baseRecode inString 
    | T.null inString = error "Null input in baseRecode"
    | T.length inString == 1 = inString
    | hasN inString = T.pack "N"
    | hasDel inString = T.pack "-"
    
    | inString == T.pack "A,C" = T.pack "M"
    | inString == T.pack "A,G" = T.pack "R"
    | inString == T.pack "A,T" = T.pack "W"
    | inString == T.pack "C,G" = T.pack "S"
    | inString == T.pack "C,T" = T.pack "Y"
    | inString == T.pack "G,T" = T.pack "K"
    | inString == T.pack "A,C,G" = T.pack "V"
    | inString == T.pack "A,C,T" = T.pack "H"
    | inString == T.pack "A,G,T" = T.pack "D"
    | inString == T.pack "C,G,T" = T.pack "B"
    | inString == T.pack "A,C,G,T" = T.pack "N"
    
    | inString == T.pack "AC" = T.pack "M"
    | inString == T.pack "AG" = T.pack "R"
    | inString == T.pack "AT" = T.pack "R"
    | inString == T.pack "CG" = T.pack "S"
    | inString == T.pack "CT" = T.pack "Y"
    | inString == T.pack "GT" = T.pack "K"
    | inString == T.pack "ACG" = T.pack "V"
    | inString == T.pack "ACT" = T.pack "H"
    | inString == T.pack "AGT" = T.pack "D"
    | inString == T.pack "CGT" = T.pack "B"
    | inString == T.pack "ACGT" = T.pack "N"
    | inString == T.pack "<>DEL" = T.pack "-"
    | otherwise = error ("Unrecognized polymorphism " ++ (T.unpack inString))

-- | filterLines removes lines that start with #
filterLines :: [T.Text] -> [T.Text]
filterLines inLines =
    if null inLines then []
    else 
        if T.head (head inLines) == '#' then filterLines  (tail inLines)
        else head inLines : filterLines (tail inLines)

-- | get4tuple reads lines from VCF file and extracts the 4 pieces of info:
-- chromosome, position, reference genome state, variant state
-- skips 3rd field
get4tuple :: T.Text -> (T.Text, T.Text, T.Text, T.Text)
get4tuple lineString =
    let parts = T.words lineString
        chromosomeNumber = parts !! 0
        position = parts !! 1
        referenceState = baseRecode $ T.pack $ sort $ filter (/= ',') $ nub $ T.unpack  (parts !! 3)
        variantState = baseRecode $ T.pack $ sort $ filter (/= ',') $ nub  $ T.unpack  (parts !! 4)
    in
    --trace (show chromosomeNumber ++ " " ++ show position ++ " " ++ show referenceState ++ " " ++ show vaiantState) 
    -- check for "monomorphic reference" where there is no variation but hasn't been filtered
    if variantState == T.pack "." then  (chromosomeNumber, position, referenceState, referenceState)
    else (chromosomeNumber, position, referenceState, variantState)

-- | splitOnChrom splits the tuples list by the first element which is the chromosome 
-- identifier 1-22, X, Y, mt
splitOnChrom :: [(T.Text, T.Text, T.Text, T.Text)] -> [[(T.Text, T.Text, T.Text, T.Text)]] -> [[(T.Text, T.Text, T.Text, T.Text)]]
splitOnChrom inTupleList curList =
   if null inTupleList then curList
    else 
        let (curChrom, _, _, _) = head inTupleList
            thisList = getChromList inTupleList curChrom
            newList = curList ++ [thisList]
        in
        splitOnChrom (drop (length thisList) inTupleList) newList

-- | getChromList make a list of all tuples untill chromosome ID changes
getChromList ::  [(T.Text, T.Text, T.Text, T.Text)] -> T.Text -> [(T.Text, T.Text, T.Text, T.Text)]
getChromList inTupleList prevChrom =
    if null inTupleList then []
    else 
        let (curChrom, _, _, _) = head inTupleList
        in
        --trace (prevChrom ++ " " ++ curChrom ++ " " ++ show (length inTupleList) ++ " " ++ pos) (
        {-if curBase == T.pack "." then getChromList (tail inTupleList) prevChrom --trace (show (T.unpack pos) ++ " ") 
        else -}
        if curChrom /= prevChrom then []
        else (head inTupleList) : getChromList (tail inTupleList) curChrom
        

-- |  fourTuple2String converts four-tuple to string for output
fourTuple2String :: (T.Text, T.Text, T.Text, T.Text) -> String
fourTuple2String (a,b,c,d) = (T.unpack a) ++ " " ++ (T.unpack b) ++ " " ++ (T.unpack c) ++ " " ++ (T.unpack d)

-- | printChrom take a strub and chromosome variant list and prints to file
-- with name stub ++ chromID ++.SNP
printChrom :: String -> [(T.Text, T.Text, T.Text, T.Text)] -> IO ()
printChrom stub variantList =
    let (chromID, _,_,_) = head variantList
        fileName = stub ++ "." ++ (T.unpack chromID) ++ ".SNP"
        stringList = fmap fourTuple2String variantList
        --stringList = parMap rdeepseq fourTuple2String variantList
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
        if (length args < 1) then errorWithoutStackTrace "At least on arg required: input VCF file.  A second arg with stub string is optional"
        else hPutStrLn stderr ("Input VCF file name: " ++ (head args))
        if (length args == 2) then hPutStrLn stderr ("File output stub name: " ++ (last args))
        else hPutStrLn stderr ("File output stub name (from nput file): " ++ (takeWhile (/= '.') (head args)))
        inFileHandle <- openFile (args !! 0) ReadMode
        inContents <- hGetContents inFileHandle
        let inLines = T.lines $ T.pack inContents
        let newLines = filterLines inLines
        --hPutStrLn stderr "Reducing Data to 4-tuples"
        let guts = fmap get4tuple newLines
        --let guts = parMap rdeepseq get4tuple newLines
        --hPutStrLn stderr "Separating into Chromosomes"
        let gutsChromList = splitOnChrom guts []
        --mapM_ (hPutStr stderr) $ fmap show guts
        --hPutStrLn stderr ("There were " ++ show (length guts) ++ " SNP variants in " ++ show (length gutsChromList) ++ " chromosomes")
        --hPutStrLn stderr "Outputting files"
        if (length args == 2) then mapM_ (printChrom (last args)) gutsChromList 
        else mapM_ (printChrom (takeWhile (/= '.') (head args))) gutsChromList 
        hPutStrLn stderr "All done"


