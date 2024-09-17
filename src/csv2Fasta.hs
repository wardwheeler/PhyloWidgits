{- |
Module      :  csv2Fasta
Description :  Converts csv language files to fastc, Assumes prefix free symbol set
Copyright   :  (c) 2015 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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
import System.Process
import System.Environment
import Debug.Trace
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Set as S
import Data.Maybe
import ReadFiles

-- need to escape '\' to '\\'
validSounds = ["a","\\'a","\\`a","\\textvbaraccent{a}","\\'{\\textvbaraccent{a}}","\\^a","\\~a",
                    "\\'{\\~a}","\\c{a}","\\u{a}","\\H{a}","\\|x{a}","{\\ae}","\\'{\\ae}","\\`{\\ae}",
                    "\\^{\\ae}","b","\\'b","\\`b","\\textvbaraccent{b}","{\\|[b}","B","\\!b","c",
                    "\\c{c}","C","d","{\\|[d}","{\\!d}","{\\:d}","D","e","\\'e","\\`e","\\textvbaraccent{e}",
                    "\\^e","\\~e","\\^{\\`e}","{\\\"e}","\\u{e}","\\c{e}","\\H{e}","\\|x{e}","@","\\'@",
                    "\\`@","\\~@","\\^@","\\H{@}","E","\\'E","\\`E","\\textvbaraccent{E}","\\^E","\\~E",
                    "\\c{E}","\\H{E}","\\|x{E}","{\\oe}","\\'{\\oe}","\\`{\\oe}","f","\\'f","g","\\'g","G",
                    "h","\\'h","i","\\'{\\i}","\\`{\\i}","\\textvbaraccent{\\i}","\\^{\\i}","\\~{\\i}",
                    "\\v{\\i}","\\H{\\i}","\\|x{\\i}","\\c{i}","\\c{\\i}","\\'{\\c{\\i}}","\\`{\\c{\\i}}",
                    "\\v{\\c{\\i}}","\\u{i}","\\u{\\i}","\\textbari","\\textiota","\\'{\\textiota}",
                    "\\`{\\textiota}","\\^{\\textiota}","\\`{\\c{\\textiota}}","j","\\'j","\\`j","k","\\'k",
                    "\\`k","l","\\'l","\\`l","{\\|[l}","m","\\'m","\\`m","{\\|[m}","\\'{\\|[m}","\\`{\\|[m}",
                    "M","n","\\'n","\\`n","{\\ng}","\\'{\\ng}","\\`{\\ng}","{\\textltailn}","o","\\'o","\\`o",
                    "\\^o","O","\\'O","\\`O","\\^O","{\\o}","\\'{\\o}","\\`{\\o}","\\c{o}","\\`{\\c{o}}",
                    "\\'{\\c{o}}","{\\!o}","{\\textcloseomega}","\\'{\\textcloseomega}","\\`{\\textcloseomega}",
                    "\\^{\\textcloseomega}","\\~\\'{\\textcloseomega}}","\\~\\`{\\textcloseomega}","p","{\\th}",
                    "F","q","r","{\\|[r}","{\\;R}","R","s","\\'s","S","t","\\'t","\\`t","\\'{\\c{t}}","T",
                    "u","\\'u","\\`u","\\v{u}","\\~u","\\'{\\~u}","\\`{\\~u}","\\'{\\c{u}}","\\`{\\c{u}}",
                    "\\v{\\c{u}}","\\^{\\c{u}}","\\u{\\c{u}}","{\\textbaru}","v","{\\textturnv}",
                    "\\'{\\textturnv}","\\`{\\textturnv}","W","w","x","{\\textchi}","y","\\'y","\\`y",
                    "\\^y","\\m{y}","z","Z","P","{s\\super h}","{k\\super h}","{p\\super h}","{m\\super b}",
                    "{d\\super y}","{p\\super 4}","{l\\super 4}","{{\\ng}\\super 4}","{m\\super 4}","{k\\super w}",
                    "{{\\textltailn}\\super w}","{n\\super d}","{d\\super w}","{-}",
                    --".", 
                    "{\\*:}",
                    "{\\textdoublebarslash}","\\'{ }","\\`{ }","\\'{-}","\\`{-}","\\`{\\c{t}}","\\`{\\c{t}}",
                    "\\^u","\\'{\\|[m}","{\\|[n}","U","\\'U","\\`U","\\^U","\\'{\\|[{\\i}}","\\textdelta",
                    "\\textchi","\\^{\\v{o}}","{\\|[{\\textiota}}","\\'{\\textbari}","\\`{\\textbari}",
                    "\\v{\\textbari}","\\'{\\textbaru}","\\`{\\textbaru}","\\^{\\textbaru}","\\~r",
                    "{s\\super 4}","{\\textbardotlessj}","{n\\super h}","{t\\super h}","{\\|[z}",
                    "{m\\super h}","{\\|[t}","{\\:r}","\\v{s}","\\c{r}","{{\\|[t}\\super h}",
                    "{{\\|[t}\\super 4}","{h\\super 4}","{k\\super 4}","{t\\super 4}","{\\|[{\\ng}}",
                    "\\~{\\^{\\i}}","\\~{\\^a}","\\~{\\v{\\i}}",
                    "{\\textipa{-}}"]

-- | 'glueNames' glueNames concatenates the name and first data value--specific to Bantu data CSV
-- and returns new name data pair
glueNames :: (String, [String]) -> (String, [String]) 
glueNames (name, dataList) = 
        let newName = name ++ (head dataList)
            newNewName = filter (not . isSpace) newName
        in 
        (newNewName, tail dataList)

-- | 'checkSymbol' check symbol sees if any members of a String list are prefix of a single String
--  | returns (True, symbol) if found (False, "") if not.
checkSymbol :: String -> [String] -> (Bool, String)
checkSymbol toCheck validList = 
    if null validList then (False, "")
    else 
        let firstSymbol = head validList
            isFound = isPrefixOf firstSymbol toCheck 
        in
        --trace ("\nCheck " ++ show firstSymbol ++ " and " ++ show toCheck ++ ":" ++ show isFound ++ "\n")
        --(
        if isFound then (True, firstSymbol)
        else checkSymbol toCheck (tail validList)
        --)

-- 'checkWord' checkWord apply checkSymbol to entire word and parses into valid symbols 
-- (True, [[String]]) or if invalid errors (returns (False, [""])
checkWord :: [String] -> [String] -> String -> (Bool, [String])
checkWord validSounds outSymbols inWordRaw =
    if null inWordRaw then (True, outSymbols)
    else
        let inWord = trim inWordRaw --can haev random spaces in String
            (firstCheck, firstSymbol) = checkSymbol inWord validSounds 
        in
        if firstCheck then checkWord validSounds (outSymbols ++ [firstSymbol]) (fromJust $ stripPrefix firstSymbol inWord) 
        else error ("\nInput sequence " ++ inWord ++ " after " ++ (concat $ intersperse "" outSymbols) ++ "  begins with a symbol sequence not in valid list")

-- 'filterExtraDoubleQuotes' removes extra double quotes added by csv export of excel
filterExtraDoubleQuotes :: String -> String
filterExtraDoubleQuotes inString =
    if null inString then inString --error "Empty string in filterExtraDoubleQuotes"
    else
        let first = head inString
            second = last inString
        in
        if ((first == '\"') && (second == '\"')) then removeDoubleDoubleQuotes (tail $ init inString) 
        else inString

-- 'removeDoubleDoubleQuotes' deletes first of 2 double quotes
removeDoubleDoubleQuotes :: String -> String
removeDoubleDoubleQuotes inString =
    if null inString then []
    else
        if length inString < 2 then inString
        else 
            let first = head inString
                second = head $ tail inString
            in
            if (first == '\"') && (second == '\"') then removeDoubleDoubleQuotes (tail inString)
            else first : (removeDoubleDoubleQuotes (tail inString))


-- | 'checkSounds' checkSounds takes paired data and a list of valid sounds 
-- and checks if there is a valid prefix to the cound sequences in the list 
-- of valid sounds. If not, then the sound is invalid.
checkSounds :: [String] -> (String, [String]) -> (String, [[String]])
checkSounds validSounds inData =
    if null validSounds then error "No sounds in list"
    else
        let (langName, langData) = inData
            --This for POY encoding
            --processedData = fmap replaceForPOY $ fmap (filter (`notElem` (editorCrap ++ whiteSpace))) langData
            processedData = fmap (filter (`notElem` (editorCrap ++ whiteSpace))) langData
            firstWordListList = fmap snd $ fmap (checkWord validSounds []) (fmap filterExtraDoubleQuotes processedData) --langData --firstWord
        in
        --trace ("\nWord " ++ firstWord ++ " -> " ++ show firstWordListList)
        (langName, firstWordListList)

-- | 'concatPair' concatPair takes a pair of string and concats the,
concatPair :: (String, String) -> String
concatPair (in1, in2) = 
    if null in2 then []
    else in1 ++ in2 ++ "\n"

-- | 'printWordFile' printWordFile outputs word lists in an ineffeicent manner 
-- using '!!'
printWordFile :: [(String, [[String]])] -> [String] -> String -> IO ()
printWordFile wordDataList fileNameList fileName =
    if null fileNameList then error "No file names"
    else if null wordDataList then error "No data to print"
    else 
        let index = fromJust $ elemIndex fileName fileNameList
            (nameList, wordLists) = unzip wordDataList 
            indexWord = fmap (!! index) wordLists
            fastlNames = fmap (++ "\n") $ fmap ('>':) nameList
            symbolLists = fmap (intercalate " ") indexWord 
            formatPair = zip fastlNames symbolLists
            formatList = fmap concatPair formatPair
        in
        do
            outFileHandle <- openFile fileName WriteMode
            mapM_ (hPutStr outFileHandle) formatList
            hClose outFileHandle

-- | 'getScriptLine' getScriptLine creates a POY script line from word name
getScriptLine :: String -> String
getScriptLine inString =
        if null inString then error "Error in getScript line--no name"
        else 
            "read (custom_alphabet:(\"" ++ inString ++ ".fastc\", tcm:(\"" ++ inString ++ ".tcm\")))"

-- | 'printScriptFile' printScriptFile prints poy line for reading files and tcm matrices
printScriptFile :: String -> [String] -> IO ()
printScriptFile scriptFileName wordNameList =
    let scriptLineList = fmap getScriptLine wordNameList
    in
    do
        outFileHandle <- openFile scriptFileName WriteMode
        mapM_ (hPutStrLn outFileHandle) scriptLineList
        hClose outFileHandle


-- | 'printTCMFile' printTCMFile outputs tcm files for POY and PCG in an ineffeicent manner 
-- using '!!'
printTCMFile :: [(String, [[String]])] -> [String] -> String -> IO ()
printTCMFile wordDataList fileNameList fileName =
    if null fileNameList then error "No file names"
    else if null wordDataList then error "No data to print"
    else 
        let index = fromJust $ elemIndex fileName fileNameList
            (nameList, wordLists) = unzip wordDataList 
            indexWord = sort $ nub $ concat $ fmap (!! index) wordLists
            --wordSets = fmap S.fromList wordLists
            --allWordSet = S.unions wordSets
            fastlNames = fmap (++ "\n") $ fmap ('>':) nameList
            symbolLists = intercalate " " indexWord 
            --formatPair = zip fastlNames symbolLists
            --formatList = fmap concatPair formatPair
            matchCost = "0 "
            subsitutionCost = "1 "
            inDelCost = "1 "
            costMatrix = getCostMatrix 0 0 (length indexWord) matchCost subsitutionCost inDelCost
        in
            --trace ("\nIndex:" ++ show (length indexWord) ++ "\n" ++ show indexWord)
                -- "\nSets:" ++ show (length wordSets) ++ " " 
                -- ++ show (length wordSets) ++ " " ++ show wordSets) 
            --(
        do
            outFileHandle <- openFile fileName WriteMode
            --mapM_ (hPutStr outFileHandle) symbolLists
            hPutStrLn outFileHandle symbolLists
            hPutStrLn outFileHandle costMatrix
            hClose outFileHandle 
            --)

-- | 'getCostMatrix' getCostMatrix produces a simple tcm matrix as big string 
getCostMatrix :: Int -> Int -> Int -> String -> String -> String -> String
getCostMatrix row column numSymbols matchCost subsitutionCost inDelCost = 
    if (row == numSymbols) && (column == numSymbols) then  matchCost ++ "\n"
    else if (column == numSymbols) then inDelCost ++ "\n" ++ (getCostMatrix (row +1) 0 numSymbols matchCost subsitutionCost inDelCost)
    else if row == numSymbols then inDelCost ++ (getCostMatrix row (column + 1) numSymbols matchCost subsitutionCost inDelCost)
    else if column < row then subsitutionCost ++ (getCostMatrix row (column + 1) numSymbols matchCost subsitutionCost inDelCost)
    else if column > row then subsitutionCost ++ (getCostMatrix row (column + 1) numSymbols matchCost subsitutionCost inDelCost)
    else if row == column then matchCost ++ (getCostMatrix row (column + 1) numSymbols matchCost subsitutionCost inDelCost)
    else error "Error in matrix string construction"

-- | 'trim' trim removes leading and trailing white space
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | 'replaceForPOY' replaceForPOY replaces '@' with 3 and '|' with 4.  These cause stream errors in OCAML
-- parser
replaceForPOY :: String -> String
replaceForPOY inString =
    if null inString then []
    else 
        let firstChar = head inString 
        in
        if firstChar == '@' then '3' : (replaceForPOY (tail inString))
        else if firstChar == '|' then '4' : (replaceForPOY (tail inString))
        else firstChar : (replaceForPOY (tail inString))

editorCrap = ['\r']
whiteSpace = [' ', '\t', '\v']

-- | 'main' Main Function to run latex IPA csv parser
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        if (length args /= 2) 
            then error "Two input files required (word csv and sound list)" 
            else hPutStr stderr "Input files: "
        mapM_ (hPutStr stderr) args
        hPutStrLn stderr ""
        inFileHandle <- openFile (head args) ReadMode
        inContents <- hGetContents inFileHandle
        soundFileHandle <- openFile (last args) ReadMode
        soundContents <- hGetContents soundFileHandle

        -- this replaces @ and | for POY processing
        --let validSoundsFromFile = filter (not . null) $ fmap replaceForPOY $ fmap (filter (`notElem` (editorCrap ++ whiteSpace))) $ fmap trim $ sort $ lines soundContents --filter spaces
        
        -- no changes here
        let validSoundsFromFile = filter (not . null) $ fmap (filter (`notElem` (editorCrap ++ whiteSpace))) $ fmap trim $ sort $ lines soundContents --filter spaces
        

        let (firstRow:inData, _) = processCsvInput inContents
        hPutStrLn stderr ("There are " ++ show (length inData) ++ " lines and " ++ show (length $ snd $ head inData) ++ " columns")
        let newData = fmap glueNames inData
        --hPutStrLn stderr ((fst $ head newData) ++  " " ++ (show $ snd $ head newData))
        let validWordList = fmap (checkSounds validSoundsFromFile) newData
        let outFileNames = fmap (++ ".fastc") $ fmap trim $ fmap (filter (/= '?')) (tail $ snd firstRow) --remove '?' so not screw up filenames
        hPutStrLn stderr ("Printing to " ++ show outFileNames)
        mapM_ (printWordFile validWordList outFileNames) outFileNames
        let tcmFileNames = fmap (++ ".tcm") $ fmap trim $ fmap (filter (/= '?')) (tail $ snd firstRow) --remove '?' so not screw up filenames
        mapM_ (printTCMFile validWordList tcmFileNames) tcmFileNames
        let scriptFileName = (takeWhile (/= '.') $ head args) ++ ".poy"
        putStrLn ("Creating script file " ++ scriptFileName)
        printScriptFile scriptFileName (fmap trim $ fmap (filter (/= '?')) (tail $ snd firstRow))
        --hPutStrLn stderr (show validWordList)
        --create and output fasta/l files
        --names of files from first row of English word names (with offending charctaers--eg ?--removed)
        --mapM_ (hPutStr stderr) (sort validSoundsFromFile)
        

