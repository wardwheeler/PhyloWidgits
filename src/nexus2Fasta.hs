--File to read nexus file and return fasta file

import Data.List
import Debug.Trace

--printFasta takes list of two strings (name and sequence) and 
--prints with '>' etc
printFasta :: [[String]] -> IO ()
printFasta stuff =
    if null stuff then putStr ""
    else 
        let curStuff = head stuff
        in
        if length curStuff < 2 then 
            do
                putStr ""
        else
            do
                putStr (">" ++ (head curStuff) ++ "\n" ++ (last curStuff) ++ "\n")
                printFasta (tail stuff)

--subRN subsitutes /n for /r so lines will work
subRN :: String -> String
subRN inString =
    if null inString then []
    else
        let inChar = head inString
        in
        if (inChar == '\r') then '\n' : (subRN (tail inString))
        else inChar : (subRN $ tail inString)


--read fasta file
main :: IO ()
main = 
  do 
     allFile <-  getContents
     --print $ length $ lines allFile 
     let allFileProcessed = subRN allFile
     let rawGuts = filter (/= '\t') $ filter (/= '\'') allFileProcessed
     let guts = lines $ takeWhile (/= ';') rawGuts -- $ unlines $ drop 8 $ lines rawGuts
     --print guts
     let wordGuts = map words guts
     --print wordGuts
     printFasta wordGuts
