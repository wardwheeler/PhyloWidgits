{- |
Module      :  ipaConvert
Description :  Inter-converts ipa alphabet symbols to be parsable by phyloAlgoInfo
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

{- THeera re a set of ccontrol charcters that can't be in ipa element symbols set
['{','}',':','"',';'] that are converted to to symbols not in ipa or phyloAlgInfo grammar
and can converted back
new set ['<','>','#','&']
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

-- | replaceFromIPA takes a character and replaces ['{','}',':','"',';'] with ['<','>','%',#','&']
replaceFromIPA :: Char -> Char
replaceFromIPA inChar =
    case inChar of
        '{' -> '<'
        '}' -> '>'
        ':' -> '%'
        '"' -> '#'
        ';' -> '&'
        _ -> inChar

-- | replaceToIPA takes a character and replaces ['<','>','%',#','&'] with ['{','}',':','"',';']  
replaceToIPA :: Char -> Char
replaceToIPA inChar =
    case inChar of
        '<' -> '{'
        '>' -> '}'
        '%' -> ':'
        '#' -> '"'
        '&' -> ';'
        _ -> inChar

-- | replaceForPMDL inter-replaces ['{','}',':','"',';'] with ['<','>','%',#','&']
replaceForPMDL :: String -> String -> String
replaceForPMDL direction inSound =
    if null inSound then []
    else 
        let newSound = if direction == "fromIPA" then fmap replaceFromIPA inSound
                       else if direction == "toIPA" then fmap replaceToIPA inSound
                       else error ("Unrecogized direction : " <> direction)
        in
        newSound 

-- | 'main' Main Function to run latex IPA csv parser
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        if (length args /= 2) 
            then errorWithoutStackTrace "Single input file (sound list) and direction of conversion fromIPA/toIPA " 
            else hPutStr stderr "Input files: "
        mapM_ (hPutStrLn stderr) args
        hPutStrLn stderr "\n"
        soundFileHandle <- openFile (head args) ReadMode
        soundContents <- hGetContents soundFileHandle
        let direction = (last args)
        if direction `notElem` ["fromIPA","toIPA"] then errorWithoutStackTrace ("Second argumentmust be in " <> (show  ["fromIPA","toIPA"]))
        else hPutStrLn stderr ("Converting " <> direction)

        -- this replaces ['{','}',':','"',';'] with ['<','>','%',#','&'] or visa versa
        -- let validSoundsFromFile = fmap (replaceForPMDL direction) $ filter (not . null) $ fmap (filter (`notElem` (editorCrap ++ whiteSpace))) $ fmap trim $ sort $ concat $ words soundContents --filter spaces

        let elementList = if direction == "fromIPA" then lines soundContents
                          else words soundContents

        let validSoundsFromFile = nub $ fmap (replaceForPMDL direction) $ filter (not . null) $ fmap (filter (`notElem` (editorCrap ++ whiteSpace))) elementList
        let formattedList = if direction == "fromIPA" then ('"' : (concat $ intersperse "\",\"" validSoundsFromFile)) <> "\""
                            else (concat $ intersperse " " validSoundsFromFile)
        
        hPutStrLn stdout formattedList 
        --create and output fasta/l files
        --names of files from first row of English word names (with offending charctaers--eg ?--removed)
        --mapM_ (hPutStr stderr) (sort validSoundsFromFile)
        

