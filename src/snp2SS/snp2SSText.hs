{- |
Module      :  snp2SSText
Description :  converts SNP data to Hennig86/TNT format
               BUT uses lazt text to help with memory consumption
               keeps character ID information in separate file.
Copyright   :  (c) 2018-2021 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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
import System.Environment
import Data.List
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO

-- | getLeaves takes a list of lines and the line begining with '#CHROM' contains the names
-- of leaves
getLeaves :: [L.Text] ->[L.Text]
getLeaves inLines =
    if null inLines then []
    else 
        let firstLine = L.words $ head inLines
            firstWord = head firstLine
        in
        if firstWord == (L.pack "#CHROM") then drop 9 firstLine
        else getLeaves (tail inLines)

-- | refineCharData converts '.' to '0'. aND OTHER STATES TO '1','2', or '3'
refineCharData :: [L.Text] -> [L.Text]
refineCharData inStringList =
    if null inStringList then []
    else let firstWord = head inStringList
    in
    if firstWord == (L.pack ".") then (L.pack "0") : (refineCharData $ tail inStringList)
    else 
        let alleleString = L.takeWhile (/= ':') firstWord
            allele0 = L.head alleleString
            allele1 = L.last alleleString
        in
        if allele0 == allele1 then (L.pack [allele0]) : (refineCharData $ tail inStringList)
        else (L.pack ("[" ++ [allele0] ++ [allele1] ++ "]")) : (refineCharData $ tail inStringList)

-- | getLineData takes an SNP line and extracts character info and state infor for terminals for that character
getLineData :: L.Text -> ([L.Text],[L.Text])
getLineData inLine =
    if L.null inLine then error "Line data empty"
    else
        let lineParts = L.words inLine
            charInfo = filter (/= (L.pack("."))) $ take 5 lineParts
            charData = drop 9 lineParts
            refinedCharData = refineCharData charData
        in
        (charInfo, refinedCharData)


-- | processFileData takes string from input SNP files and returns a pair of character information 
-- and charracter assignment data
processFileData :: L.Text -> [([L.Text], [L.Text])]
processFileData fileString =
    if L.null fileString then error "File contents empty"
    else
        let inLines = L.lines fileString
        in
        fmap getLineData inLines

-- | joinLists adds lists together pairwise
joinListsText :: [L.Text] -> [L.Text] -> [L.Text]
joinListsText first second =
    if length first /= length second then error ("Input lists not same length: " ++ (show $ length first) ++ " leaves " ++ (show $ length second) ++ " Characters")
    else 
        if null first then [L.empty]
        else
            let firstName = head first
                firstChars =head second
            in
            --trace (firstName ++ " " ++ firstChars ++ "\n")
           (L.append (L.snoc firstName '\n') (L.snoc firstChars '\n')) : (joinListsText (tail first) (tail second)) 

-- | joinLists adds lists together pairwise
joinLists :: [String] -> [L.Text] -> String
joinLists first second =
    if length first /= length second then error ("Input lists not same length: " ++ (show $ length first) ++ " leaves " ++ (show $ length second) ++ " Characters")
    else 
        if null first then []
        else
            let firstName = head first
                firstChars = L.unpack $ head second
            in
            --trace (firstName ++ " " ++ firstChars ++ "\n")
            (firstName ++ "\t" ++ firstChars ++ "\n") ++ (joinLists (tail first) (tail second)) 

-- | getHennigString takes leaf list and taxon char list to create output string 
-- with Hennig/TNT format
getHennigString :: Int -> [String] -> [L.Text] -> String
getHennigString numChars leafNames charCodings =
    if null leafNames then error "Empty leaf names"
    else if null charCodings then "Empty character codings"
    else 
        let joinedNameCharacters = joinLists leafNames charCodings
            firstPart = ("xread\n'SNP data'\n" ++ (show numChars) ++ " " ++ (show $ length leafNames) ++ "\n")
            lastPart = (";\ncc - 0." ++ (show (numChars - 1)) ++ ";\nproc /;\n")
        in
        firstPart ++ joinedNameCharacters ++ lastPart

-- | 'main' Main Function 
main :: IO ()
main = 
    do
        --get file stubname for outputs, "HEAD" file with taxon info (line 27 starting with "#CHROM"),
        --then additional input files 
        args <- getArgs
        if (length args < 3) then error "At least two input file required--HEAD and SNP"
        else do
            hPutStrLn stderr ("File output stub name: " ++ (head args))
            hPutStrLn stderr ("There are " ++ (show $ length $ tail args) ++ " input files")
            mapM_ (hPutStrLn stderr) (tail args)
        inFileHandle <- openFile (args !! 1) ReadMode
        inContents <- hGetContents inFileHandle
        let inLines = L.lines $ L.pack inContents
        let leafList = mapM L.unpack (getLeaves inLines)
        hPutStrLn stderr ("There are " ++ (show $ length leafList) ++ " taxa in data set")
        mapM_ (hPutStr stderr) $ fmap (++ " " ) leafList
        hPutStrLn stderr "\n"
        dataFileContentsList <- mapM readFile (tail $ tail args)
        let (_, charCodings) = unzip $ concat $ fmap processFileData $ fmap L.pack dataFileContentsList

        -- Output character info with char numbers in separate file

        -- Transpose character coding so rows are by taxon as opposed to columns
        -- then reverse so outgroups is first --specific to the dat set this is writen for
        --hPutStrLn stderr (show $ charCodings !! 0)
        {-
        let numCharacters = length charCodings
        hPutStrLn stderr ("There are " ++ show numCharacters ++ " characters")
        -}
        let taxonCharCodings = transpose charCodings
        --hPutStrLn stderr (show $ taxonCharCodings !! 0)

        -- Write charcater contents to temp file.
        let fileGutsList = joinListsText (fmap L.pack leafList) (fmap L.concat taxonCharCodings)
        mapM_ (LIO.appendFile ((head args) ++ ".tmp")) fileGutsList
        --read tempfile and get charcater number

        -- output final Hennig file


        {-
        let hennigString = getHennigString numCharacters (reverse leafList) (reverse $ fmap L.concat taxonCharCodings)
        hPutStr stdout hennigString
        -}
        hPutStrLn stderr "All done"


