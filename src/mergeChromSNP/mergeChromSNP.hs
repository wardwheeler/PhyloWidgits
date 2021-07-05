{- |
Module      :  mergeChromSNP
Description :  inputs multiple SNP files created by SNP2Chrom fomr VCF files
			   of individual genome SNP variants
			   outputs a TNT/POY/PCG legible file for phylogenertic analysis
			   checks that Chromosome IDs are same for input files (otherwise
			   position infomation would not make sense)
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
import Data.Char
import Data.Maybe
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO
import Data.Function (on)


-- | line2FourTuple converts listof L.Text to 4-tuple of L.Text
line2FourTuple :: [L.Text] -> (L.Text, Int, L.Text, L.Text)
line2FourTuple inList =
    if null inList then error "Null list in line2FourTuple"
    else
       (inList !! 0,read (L.unpack (inList !! 1)) :: Int,  inList !! 2,inList !! 3) 

-- | getAllFilesContents reads inputs of all files and converts the 
-- contents to lists of lists 4-tuples of L.Text
getAllFilesContents :: String -> IO [(L.Text,Int,L.Text,L.Text)]
getAllFilesContents first = do
       inFileHandle <- openFile first ReadMode
       inContents <- hGetContents inFileHandle
       let inLineWords = fmap L.words $ L.lines $ L.pack inContents
       let fourTupleList = fmap line2FourTuple inLineWords
       return fourTupleList

-- | createAndVerifyReferenceSet takes input list of all tuples from input files
-- and verifies that all chromosome IDs are same
-- if not--erorrs with file name and chromosome ID
-- returns list of reference states and position
-- preserves ordering
createAndVerifyReferenceSet :: L.Text -> [(L.Text,Int,L.Text,L.Text)] -> [(Int,L.Text)]
createAndVerifyReferenceSet inChromeID inList =
    if null inList then [] -- sort on fst
    else 
        let (chromID, position, refState, varState) = head inList
        in
        if varState == L.pack "." then createAndVerifyReferenceSet inChromeID (tail inList)
        else if chromID /= inChromeID then error ("Chromosome ID " ++ show chromID ++ " is not same as first " ++ show inChromeID)
        else (position, refState) : createAndVerifyReferenceSet inChromeID (tail inList)

-- | createVariantSet pulls postition and variant state from input tuples
createVariantSet :: [(L.Text,Int,L.Text,L.Text)] -> [(Int,L.Text)]
createVariantSet inList =
    if null inList then [] -- sort on fst
    else 
        let (_, position, _, varState) = head inList
        in
        if varState == L.pack "." then createVariantSet (tail inList)
        else (position, varState) : createVariantSet (tail inList)

-- | makeTextRow takes refernce states and input leaf states and cretes a data row filling in
-- states from reference states if there is no variant SNP at a position
makeTextRow :: [(Int,L.Text)] -> [(Int,L.Text)] -> L.Text
makeTextRow refererenceList terminalList =
    if null refererenceList then L.empty
    else 
        let (refPos, refState) = head refererenceList
        in 
        if null terminalList then L.cons (L.head refState) (makeTextRow (tail refererenceList) terminalList)
        else 
            let (varPos, varState) = head terminalList
            in
            if refPos == varPos then L.cons (L.head varState) (makeTextRow (tail refererenceList) (tail terminalList))
            else L.cons (L.head refState) (makeTextRow (tail refererenceList) terminalList)

-- | makeRowString takes a (String, Text) pair and turns it into a String for output
makeRowString :: (String, L.Text) -> String
makeRowString (inName, inText) = inName ++ " " ++ L.unpack inText

-- | nubOrdered is a nub function that requires that the input list is already ordered.  Hence,
-- repeated elements are adjacent.  This should be O(n) versus O(n^2) for nub.
nubOrdered :: (Eq a) => a -> [a] -> [a]
nubOrdered prevValue inList =
    if null inList then []
    else 
        let first = head inList
        in
        if first == prevValue then nubOrdered prevValue (tail inList)
        else first : nubOrdered prevValue (tail inList)

-- | mergeOrderedNubb takes two sorted (low to high) lists of (Int, a) pairs
-- and returns a single ordered list that is merged form the two with dupes removed
-- reverses list ordering
-- Note assumes ORDERED INPUT (low to high)
mergeOrderedNubb :: (Eq a) => [(Int, a)] -> [(Int, a)] -> [(Int,a)]
mergeOrderedNubb firstList secondList 
    | null firstList && null secondList     = []
    | null firstList                        = secondList
    | null secondList                       = firstList
    | (head firstList) == (head secondList) = (head firstList) : mergeOrderedNubb (tail firstList) (tail secondList)
    | (fst $ head firstList) < (fst $ head secondList) = (head firstList) : mergeOrderedNubb (tail firstList) secondList
    | (fst $ head firstList) > (fst $ head secondList) = (head secondList) : mergeOrderedNubb firstList (tail secondList)
    | otherwise = error ("This can't happen in mergeOrderedNubb")

-- | makeFastaString take (name, sequence) pair and outputs fasta for input as aligned sequences
makeFastaString :: (String, L.Text) -> String
makeFastaString (inName, inText) = ">" ++ inName ++ "\n" ++ (L.unpack inText) ++ "\n"

-- | dna2Int converts DNA characters to Int repreentaition with TNT ambiguity codes
dna2Int :: L.Text -> L.Text
dna2Int dnaText 
    | L.null dnaText = L.empty
    | (L.head dnaText) == 'A' = L.cons '0' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'C' = L.cons '1' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'G' = L.cons '2' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'T' = L.cons '3' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == '-' = L.cons '4' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'M' = L.cons '[' $ L.cons '0' $ L.cons '1' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'R' = L.cons '[' $ L.cons '0' $ L.cons '2' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'W' = L.cons '[' $ L.cons '0' $ L.cons '3' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'S' = L.cons '[' $ L.cons '1' $ L.cons '2' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'Y' = L.cons '[' $ L.cons '1' $ L.cons '3' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'K' = L.cons '[' $ L.cons '2' $ L.cons '3' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'V' = L.cons '[' $ L.cons '0' $ L.cons '1' $ L.cons '2' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'H' = L.cons '[' $ L.cons '0' $ L.cons '1' $ L.cons '3' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'D' = L.cons '[' $ L.cons '0' $ L.cons '2' $ L.cons '3' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'B' = L.cons '[' $ L.cons '1' $ L.cons '2' $ L.cons '3' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | (L.head dnaText) == 'N' = L.cons '[' $ L.cons '0' $ L.cons '1' $ L.cons '2' $ L.cons '3' $ L.cons ']' (dna2Int $ L.tail dnaText) 
    | otherwise = error ("Unrecognized polymorphism " ++ [(L.head dnaText)])


-- | 'main' Main Function 
main :: IO ()
main = 
    do
        --get input file format, inpyt files for creation of merged file
        args <- getArgs
        if (length args < 3) then error "Minimum of 3 arguments required output format (TNT or FASTA) and at least two input files are required"
        else if (head args) /= "TNT" && (head args) /= "FASTA" then error ("Output format was " ++ (head args) ++ ", but must be either TNT or FASTA")
        else do
            hPutStrLn stderr ("Output format: " ++ (head args))
            hPutStrLn stderr ("Input SNP files:")
            mapM_ (hPutStrLn stderr) (tail args)

        filesContents <- mapM getAllFilesContents (tail args)

        --create reference set from all inputs
        let (chromID, _,_,_) = head $ head filesContents
        --let referenceSNP = nubOrdered (-1, L.empty) $ sortBy (compare `on` fst) $ createAndVerifyReferenceSet chromID (concat filesContents)
        let referenceSNPList = fmap (createAndVerifyReferenceSet chromID) filesContents
        let referenceSNP = foldl' mergeOrderedNubb (head referenceSNPList) (tail referenceSNPList)


        --mapM_ (hPutStr stderr) (fmap show referenceSNP)

        --Create Text for each data row--should be in same order as reference data
        let pairList = fmap createVariantSet filesContents
        let textList = fmap (makeTextRow referenceSNP) pairList

        --Write the outpuot data file in IUPAC
        let terminalNameList = fmap (takeWhile (/= '.')) (tail args)
        if (head args == "TNT") then do
            let dataPair = zip terminalNameList (fmap dna2Int textList)
            let dataPairStrings = fmap makeRowString dataPair
            hPutStr stdout ("xread\n'Genomic SNP data'\n" ++ show (length referenceSNP) ++ " " ++ show (length terminalNameList) ++ "\n")
            mapM_ (hPutStrLn stdout) dataPairStrings
            hPutStr stdout (";\ncc . -;\nproc /;\n")
        else do
            let dataPair = zip terminalNameList textList
            let fastaStrings = fmap makeFastaString dataPair
            mapM_ (hPutStrLn stdout) fastaStrings

        --say goodbye
        hPutStrLn stderr ("There were " ++ show (length filesContents) ++ " input SNP files and " ++ show (length referenceSNP) ++ " SNPs")
        hPutStrLn stderr "All done"


