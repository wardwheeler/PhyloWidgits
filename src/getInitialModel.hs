{- |
Module      :  getElements
Description :  Creates initila model and parameter estimates from fasta/c file
                Neyman and GTR stub models
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

{- 
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

-- | 'trim' trim removes leading and trailing white space
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

editorCrap = ['\r']
whiteSpace = [' ', '\t', '\v']

{-getElementNumber
-}
getElementNumber :: String -> String -> Int
getElementNumber elementType lineString =
    if elementType == "fastc" then length $ words lineString
    else if elementType == "fasta" then 
                length $ filter (/= ' ') lineString
    else errorWithoutStackTrace ("Sequence type must be 'fasta' or 'fastc:" <>  elementType) 

{- getSymbols gets single or multiple characters elements from line
-}
getSymbols :: String -> String -> [String]
getSymbols elementType lineString =
    if elementType == "fastc" then words lineString
    else if elementType == "fasta" then 
                fmap (:[]) $ filter (/= ' ') lineString
    else errorWithoutStackTrace ("Sequence type must be 'fasta' or 'fastc:" <>  elementType) 


{- makeNeymanModelString makes a phyComplexity Neyman model 
-}
makeNeymanModelString :: String -> [String] -> String
makeNeymanModelString modelName inElements =
    if null inElements then errorWithoutStackTrace "No elements to make model alphabet"
    else 
        let elementsString = concat $ intersperse "\",\"" inElements
            alphString = "[\"" <> elementsString <> "\"]"
        in
        "\nBlockModel " <> modelName <> " {\n" <>
        "\tAlphabet : " <> alphString <> ";\n" <> "\tBranchLength : exponential: 10.0;\n" <> "\tPrecision : 1000;\n"
            <> "\tRateModifiers  : None;\n" <> "\tChangeModel :Neyman;\n" <> "\tLength : 1;\n"
            <> "};\n"

{- makeGTRModelString makes a phyComplexity GTR model with 1.0 values for everything
-}
makeGTRModelString :: String -> [String] -> String
makeGTRModelString modelName inElements =
    if null inElements then errorWithoutStackTrace "No elements to make model alphabet"
    else 
        let elementsString = concat $ intersperse "\",\"" inElements
            alphString = "[\"" <> elementsString <> "\"]"
            piFreq = show $ 1.0 / (fromIntegral $ length inElements)  
            modelPart1 = "\tChangeModel :GTR : (PiVector:["
            modelPart2 = concat $ intersperse "," $ replicate (length inElements) piFreq
            modelPart3 = "], RMatrix:["
            modelPart4 = concat $ intersperse "," $ replicate (length inElements) (show  1.0) 
            modelPart5 = concat $ intersperse "," $ replicate (length inElements) ("[" <> modelPart4 <> "]")
            modelPart6 = "]);\n"
        in
        "\nBlockModel " <> modelName <> " {\n" <>
        "\tAlphabet : " <> alphString <> ";\n" <> "\tBranchLength : exponential: 10.0;\n" <> "\tPrecision : 1000;\n"
            <> "\tRateModifiers  : None;\n" <> "\tChangeModel :GTR;\n" <> "\tLength : 1;\n"
            <> modelPart1 <> modelPart2 <> modelPart3 <> modelPart5 <> modelPart6
            <> "};\n"
            
removeFASTCAmbiguity :: String -> String
removeFASTCAmbiguity inFASTCString =
    if ('[' `elem` inFASTCString) && (']' `elem` inFASTCString) then []
    else inFASTCString

-- | 'main' Main Function to run latex IPA csv parser
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        if (length args /= 3) 
            then errorWithoutStackTrace "Require three arguments:\n\tSingle input file (fasta/c),\n\t'fasta', 'fastc', or 'auto' for single or multiple character element formats or auto-detect,\n\tand stub for output model name" 
            else hPutStrLn stderr "Input args: "
        mapM_ (hPutStrLn stderr) (fmap ('\t':) args)
        --hPutStrLn stderr "\n"

        soundFileHandle <- openFile (head args) ReadMode
        soundContents <- hGetContents soundFileHandle

        let fileOption = args !! 1
        let outStub = args !! 2

        let neyModelFile = outStub <> ".neyModel"
        let gtrModelFile = outStub <> ".gtrModel"

        let sequenceLines = filter (('>' /=) . head) $ filter (not . null) $ lines soundContents

        -- If auto try to detect fasta/fastc

        let numSpaces = length $ concat $ fmap (filter (== ' ')) sequenceLines
        let numLines = length sequenceLines

        -- hPutStrLn stderr ("lines: " <> (show numLines) <> " spaces: " <> (show numSpaces))

        let fileType = if (numSpaces < 3 * numLines) then "fasta"
                       else "fastc"
        
        hPutStrLn stderr ("Parsing as " <> fileType) 
        if fileOption == "auto" then hPutStrLn stderr ("\tIf this is not correct then specify file type in command line.")
        else hPutStr stderr ""


        let allElements = concat $ fmap (getSymbols fileType) sequenceLines

        -- checks for non-single character, non-IUPAC codes
        let multipleCharElements = 1 < (maximum $ fmap length allElements)
        let notNucAA = (0 < (length $ filter (`elem` ["J", "O"]) allElements)) || multipleCharElements

        --hPutStrLn stderr (show (multipleCharElements, notNucAA)) 

        -- check for IUPAC codes and filter out ambiguities
        let nucleotides = length $ filter (`elem` ["A", "C", "G", "T", "U", "-"]) allElements

        let aminoOnly = 0 < (length $ filter (`elem` ["E", "F", "I", "L", "P", "Q", "Z"]) allElements)

        let aminoAcids = length $ filter (`elem` ["B", "D", "E", "F", "H", "I", "K", "L", "M", "P", "Q", "R", "S", "V", "W", "X", "Y", "Z", "-"]) allElements

        let isNucleotide = (not notNucAA) && nucleotides > (quot (length allElements) 2) 
        let isAminoAcid = (not notNucAA) && ((aminoAcids > (quot (length allElements) 4)) || aminoOnly)
        

        -- test amino first since acgt and - are also aa
        let allElements' = if isNucleotide then
                                filter (`notElem` ["R", "Y", "S", "W", "M", "K", "H", "B", "V", "D","N", "?"]) allElements
                           else if isAminoAcid then
                                filter (`notElem` ["B", "Z", "X", "?"]) allElements 
                           else if multipleCharElements then
                                -- remove fastc ambiguities
                                fmap removeFASTCAmbiguity allElements
                           else allElements

        if isNucleotide then hPutStrLn stderr "Sequences are assumed to be nucleic acids and ambiguities are not counted"
        else if isAminoAcid then hPutStrLn stderr "Sequences are assumed to be amino acids and ambiguities are not counted"
        else hPutStrLn stderr "Sequences are assumed to be neither amino acids nor nucleic acids"

        let elementLengths = fmap (getElementNumber fileType) sequenceLines

        let rangeLength = (maximum elementLengths) - (minimum elementLengths)

        let averageLength = round $ (fromIntegral $ sum elementLengths) / (fromIntegral $ length sequenceLines)

        --hPutStrLn stderr ("Warning--filtering out gap ('-') characters in " <> elementFile)
        let uniqueElements = ["-"] <>  (nub $ filter (/= "-") $ sort $ allElements)

        --tail for remove gaps since added to front
        let formattedList = concat $ tail $ intersperse " " uniqueElements

        --hPutStrLn stderr ("Total elements: " <> (show $ length allElements))
        --hPutStrLn stderr ("Unique elements: " <> (show $ length uniqueElements))
        --hPutStrLn stderr ("Average length: " <> (show averageLength))
        --hPutStrLn stderr ("Range length: " <> (show rangeLength))

        -- output files 
        neyModelFileHandle <- openFile neyModelFile WriteMode
        gtrModelFileHandle <- openFile gtrModelFile WriteMode
        
        hPutStrLn stderr ("Output to files: " <> " " <> neyModelFile <> " " <> gtrModelFile)

        --hPutStrLn stderr ("Warning--filtering out gap ('-') characters in " <> elementFile)

        --hPutStrLn elementFileHandle formattedList 

        let neymanModelString = makeNeymanModelString neyModelFile uniqueElements
        hPutStrLn neyModelFileHandle neymanModelString

        let gtrModelString = makeGTRModelString gtrModelFile uniqueElements
        hPutStrLn gtrModelFileHandle gtrModelString

        hClose neyModelFileHandle
        hClose gtrModelFileHandle
        

