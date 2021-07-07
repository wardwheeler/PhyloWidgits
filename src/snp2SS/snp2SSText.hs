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
import Debug.Trace

-- | getLeaves takes a list of lines and the line begining with '#CHROM' contains the names
-- of leaves
getLeaves :: [L.Text] -> ([L.Text],[L.Text])
getLeaves inLines =
    if null inLines then ([], [])
    else 
        let firstLine = L.words $ head inLines
            firstWord = head firstLine
        in
        if firstWord == (L.pack "#CHROM") then (drop 9 firstLine, tail inLines)
        else  getLeaves (tail inLines)
        

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
            allele0' = if allele0 == ('.') then ('0')
                       else allele0
            allele1' = if allele1 == ('.') then ('0')
                       else allele1 
        in
        if allele0' == allele1' then (L.pack [allele0']) : (refineCharData $ tail inStringList)
        else (L.pack ("[" ++ [allele0'] ++ [allele1'] ++ "]")) : (refineCharData $ tail inStringList)

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
processFileData :: [L.Text] -> [([L.Text], [L.Text])]
processFileData fileStringList =
    if null fileStringList then error "File contents empty"
    else
        fmap getLineData fileStringList

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

-- | getHennigStringTuple same as getHennigString but with args as tuple
getHennigStringTuple :: (Int, [String], [L.Text]) -> String
getHennigStringTuple (numChars, leafNames, charCodings) =
    if null leafNames then error "Empty leaf names"
    else if null charCodings then "Empty character codings"
    else 
        let joinedNameCharacters = joinLists leafNames charCodings
            firstPart = ("xread\n'SNP data'\n" ++ (show numChars) ++ " " ++ (show $ length leafNames) ++ "\n")
            lastPart = (";\ncc - 0." ++ (show (numChars - 1)) ++ ";\nproc /;\n")
        in
        firstPart ++ joinedNameCharacters ++ lastPart

-- | splitLinesByChromosome takes list of data lines and splits into list where each chromosome (first field)
--   is a separate list of lines
splitLinesByChromosome :: [L.Text] -> L.Text -> [L.Text] -> [[L.Text]]
splitLinesByChromosome inLineList lastChrom currentChromList=
    if null inLineList then []
    else 
        let firstLine = head inLineList
            thisChrom = head $ L.words firstLine
        in
        if thisChrom == lastChrom then  splitLinesByChromosome (tail inLineList) thisChrom (firstLine : currentChromList)
        else
            -- trace ("New Chromosome: " ++ show thisChrom) 
            (reverse currentChromList) : splitLinesByChromosome (tail inLineList) thisChrom []

-- | writeString takes a string, stub, as suffix and an index as a tuple and writes the string to
-- the unique stub+index file name 
writeString :: (String, String, String, Int) -> IO ()
writeString (contentString, stub, suffix, index) =
    let fileName = stub ++ ("_") ++ (show index) ++ "." ++ suffix
    in
    do
        hPutStrLn stderr ("Writing " ++ fileName)
        outFileHandle <- openFile fileName WriteMode
        hPutStr outFileHandle contentString
        hClose outFileHandle

-- | 'main' Main Function 
main :: IO ()
main = 
    do
        --get file stubname for outputs, "HEAD" file with taxon info (line 27 starting with "#CHROM"),
        --then additional input files 
        args <- getArgs
        if (length args < 2) then error "Must specify file stub string and a single input file--VCF/SNP"
        else do
            hPutStrLn stderr ("File output stub name: " ++ (head args))
            hPutStrLn stderr (last args)
        inFileHandle <- openFile (args !! 1) ReadMode
        inContents <- LIO.hGetContents inFileHandle
        let inLines = L.lines inContents
        -- let leafList = mapM L.unpack (getLeaves inLines)
        let (leafList, restLines) = getLeaves inLines
        hPutStrLn stderr ("There are " ++ (show $ length leafList) ++ " taxa in data set")

        -- Split lines by chromosome to crearte multiple list of lines
        -- this to save on memory footprint for transposing and creating output file(s) later.
        let chromosomeLineSetList = splitLinesByChromosome restLines (L.pack "-1") [] 
        hPutStrLn stderr ("There are " ++ (show $ length chromosomeLineSetList) ++ " chromosomes in data set.")
        -- hPutStrLn stderr (show chromosomeLineSetList)

        -- Process each chomosome set of lines as separate file

        let charCodingsList = fmap snd $ fmap unzip $ fmap processFileData (tail chromosomeLineSetList)

        -- Output character info with char numbers in separate file

        -- Transpose character coding so rows are by taxon as opposed to columns
        -- then reverse so outgroups is first --specific to the dat set this is writen for
        --hPutStrLn stderr (show $ charCodings !! 0)
        
        let numCharactersList = fmap length charCodingsList
        -- hPutStrLn stderr ("There are " ++ show numCharactersList ++ " characters")
        
        
        let taxonCharCodingsList = fmap transpose charCodingsList
        --hPutStrLn stderr (show $ taxonCharCodings !! 0)

        -- Write charcater contents to temp file.
        -- let fileGutsList = joinListsText leafList (fmap L.concat taxonCharCodings)
        -- mapM_ (LIO.appendFile ((head args) ++ ".tmp")) fileGutsList
        --read tempfile and get charcater number

        -- output final Hennig file

        
        -- Can then reverse so outgroups is first --if the data set like that
        let hennigInfoTuple = zip3 numCharactersList (replicate (length chromosomeLineSetList) (fmap L.unpack leafList)) (fmap (fmap L.concat) taxonCharCodingsList)
        --let hennigString  = getHennigString numCharacters (fmap L.unpack leafList) (fmap L.concat taxonCharCodings)
        -- hPutStr stdout hennigString

        let hennigStringList = fmap getHennigStringTuple hennigInfoTuple
        let printTupleList = zip4 hennigStringList (replicate (length chromosomeLineSetList) (head args)) (replicate (length chromosomeLineSetList) "ss") [1..(length chromosomeLineSetList)]
        --mapM_ (hPutStr stdout) hennigStringList
        mapM_ writeString printTupleList
        
        
        hPutStrLn stderr "All done"


