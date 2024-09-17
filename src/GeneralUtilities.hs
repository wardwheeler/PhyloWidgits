{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Description :  Module with useful functions
Copyright   :  (c) 2021 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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
-}
module GeneralUtilities (
    module GeneralUtilities,
) where

import Control.DeepSeq
import Control.Monad
import Data.Array
import Data.Array.IO
import Data.BitVector.LittleEndian qualified as BV
import Data.Bits
import Data.Char
import Data.Foldable
import Data.List (partition, sortOn)
import Data.List qualified as L
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Time.Clock.POSIX
import Data.Vector qualified as V
import Data.Word
-- imports for traceNoLFIO
import Foreign.C.String
import Numeric
import System.IO.Unsafe
import System.Random
import Text.Read


{- | traceNOLF is trace modified from Debug/Trace to not have
a line feed (\n) after message
-}
traceNoLF ∷ String → a → a
traceNoLF string expr = unsafePerformIO $ do
    traceNoLFIO string
    pure expr


{- | traceNOLFIO is traceIO modified from Debug/Trace to not have
a line feed (\n) after message
-}
traceNoLFIO ∷ String → IO ()
traceNoLFIO msg =
    withCString "%s" $ \cfmt → do
        -- NB: debugBelch can't deal with null bytes, so filter them
        -- out so we don't accidentally truncate the message.  See #9395
        let (nulls, msg') = partition (== '\0') msg
        withCString msg' $ \cmsg →
            debugBelch cfmt cmsg
        unless (null nulls)
            . withCString "WARNING: previous trace message had null bytes"
            $ \cmsg →
                debugBelch cfmt cmsg


-- don't use debugBelch() directly, because we cannot call varargs functions
-- using the FFI.
foreign import ccall unsafe "HsBase.h debugBelch2"
    debugBelch ∷ CString → CString → IO ()


-- | functions for triples, quadruples
fst3 ∷ (a, b, c) → a
fst3 (d, _, _) = d


snd3 ∷ (a, b, c) → b
snd3 (_, e, _) = e


thd3 ∷ (a, b, c) → c
thd3 (_, _, e) = e


fst4 ∷ (a, b, c, d) → a
fst4 (e, _, _, _) = e


snd4 ∷ (a, b, c, d) → b
snd4 (_, e, _, _) = e


thd4 ∷ (a, b, c, d) → c
thd4 (_, _, e, _) = e


fth4 ∷ (a, b, c, d) → d
fth4 (_, _, _, f) = f


fst5 ∷ (a, b, c, d, e) → a
fst5 (e, _, _, _, _) = e


snd5 ∷ (a, b, c, d, e) → b
snd5 (_, e, _, _, _) = e


thd5 ∷ (a, b, c, d, e) → c
thd5 (_, _, e, _, _) = e


fth5 ∷ (a, b, c, d, e) → d
fth5 (_, _, _, e, _) = e


fft5 ∷ (a, b, c, d, e) → e
fft5 (_, _, _, _, e) = e


fst6 ∷ (a, b, c, d, e, f) → a
fst6 (e, _, _, _, _, _) = e


snd6 ∷ (a, b, c, d, e, f) → b
snd6 (_, e, _, _, _, _) = e


thd6 ∷ (a, b, c, d, e, f) → c
thd6 (_, _, e, _, _, _) = e


fth6 ∷ (a, b, c, d, e, f) → d
fth6 (_, _, _, e, _, _) = e


fft6 ∷ (a, b, c, d, e, f) → e
fft6 (_, _, _, _, e, _) = e


six6 ∷ (a, b, c, d, e, f) → f
six6 (_, _, _, _, _, e) = e


-- from https://gist.github.com/thealmarty/643c0509dc6e7e4ad6bcd05e7dbb0e44

{- | The 'zipWith8' function takes a function which combines eight
elements, as well as eight lists and returns a list of their point-wise
combination, analogous to 'zipWith'.
-}
zipWith8
    ∷ (a → b → c → d → e → f → g → h → i)
    → [a]
    → [b]
    → [c]
    → [d]
    → [e]
    → [f]
    → [g]
    → [h]
    → [i]
zipWith8 z (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) (g : gs) (h : hs) =
    z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ = []


{- | The 'zip8' function takes eight lists and returns a list of
eight-tuples, analogous to 'zip'.
-}
zip8
    ∷ [a]
    → [b]
    → [c]
    → [d]
    → [e]
    → [f]
    → [g]
    → [h]
    → [(a, b, c, d, e, f, g, h)]
zip8 = zipWith8 (,,,,,,,)


{- | The 'zipWith9' function takes a function which combines nine
elements, as well as eight lists and returns a list of their point-wise
combination, analogous to 'zipWith'.
-}
zipWith9
    ∷ (a → b → c → d → e → f → g → h → i → j)
    → [a]
    → [b]
    → [c]
    → [d]
    → [e]
    → [f]
    → [g]
    → [h]
    → [i]
    → [j]
zipWith9 z (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) (g : gs) (h : hs) (i : is) =
    z a b c d e f g h i : zipWith9 z as bs cs ds es fs gs hs is
zipWith9 _ _ _ _ _ _ _ _ _ _ = []


{- | The 'zip9' function takes nine lists and returns a list of
nine-tuples, analogous to 'zip'.
-}
zip9
    ∷ [a]
    → [b]
    → [c]
    → [d]
    → [e]
    → [f]
    → [g]
    → [h]
    → [i]
    → [(a, b, c, d, e, f, g, h, i)]
zip9 = zipWith9 (,,,,,,,,)


-- | showBits cponverts Value to bits as Srting
showBits ∷ Word64 → String
showBits inVal = showIntAtBase 2 intToDigit inVal ""


-- | showBitsV shoiw vector of bits
showBitsV ∷ V.Vector Word64 → String
showBitsV = foldMap (<> " ") . V.toList . fmap showBits


{- | doubleAsInt takes floor and ceil of Double and retuns Maybe Int
nothing if not, Just Int if it is
-}
doubleAsInt ∷ Double → Maybe Int
doubleAsInt inDouble =
    if ceiling inDouble /= floor inDouble
        then Nothing
        else Just (floor inDouble ∷ Int)


-- | doubleIsInt returns True if Double is an integer
doubleIsInt ∷ Double → Bool
doubleIsInt inDouble = ceiling inDouble == floor inDouble


-- | doubleIsInt1 returns True if Double is = Integer 1
doubleIsInt1 ∷ Double → Bool
doubleIsInt1 inDouble =
    ceiling inDouble == 1 && floor inDouble == 1


{- | editDistance is a naive edit distance between two lists
takes two  lists and returns edit distance
-}

--- from  https://wiki.haskell.org/Edit_distance
editDistance ∷ (Eq a) ⇒ [a] → [a] → Int
editDistance xs ys = table ! (m, n)
    where
        (m, n) = (length xs, length ys)
        x = array (1, m) (zip [1 ..] xs)
        y = array (1, n) (zip [1 ..] ys)

        table ∷ Array (Int, Int) Int
        table = array bnds [(ij, dist ij) | ij ← range bnds]
        bnds = ((0, 0), (m, n))

        dist (0, j) = j
        dist (i, 0) = i
        dist (i, j) =
            minimum
                [ table ! (i - 1, j) + 1
                , table ! (i, j - 1) + 1
                , if x ! i == y ! j then table ! (i - 1, j - 1) else 1 + table ! (i - 1, j - 1)
                ]


-- | checkCommandArgs takes comamnd and args and verifies that they are in list
checkCommandArgs ∷ String → [String] → [String] → Bool
checkCommandArgs commandString commandList permittedList = case commandList of
    [] → True
    firstCommand : otherCommands | firstCommand `elem` permittedList → checkCommandArgs commandString otherCommands permittedList
    firstCommand : _ →
        let errorMatch = snd $ getBestMatch (maxBound ∷ Int, "no suggestion") permittedList firstCommand
        in  errorWithoutStackTrace $
                fold
                    [ "\nError: Unrecognized '"
                    , commandString
                    , "' option. By '"
                    , firstCommand
                    , "' did you mean '"
                    , errorMatch
                    , "'?\n"
                    ]


{- | getBestMatch compares input to allowable commands and checks if in list and if not outputs
closest match
call with (maxBound :: Int ,"no suggestion") commandList inString
-}
getBestMatch ∷ (Int, String) → [String] → String → (Int, String)
getBestMatch currBest@(minDist, _) allowedStrings inString = case allowedStrings of
    [] → currBest
    candidate : cs → case editDistance candidate inString of
        0 → (0, candidate)
        candidateEditCost →
            let nextBest = case candidateEditCost `compare` minDist of
                    LT → (candidateEditCost, candidate)
                    _ → currBest
            in  getBestMatch nextBest cs inString


-- | getCommandErrorString takes list of non zero edits to allowed commands and reurns meaningful error string
getCommandErrorString ∷ [(Int, String, String)] → String
getCommandErrorString = \case
    [] → ""
    (_, firstCommand, firstMatch) : rest →
        let firstError = fold ["\tBy \'", firstCommand, "\' did you mean \'", firstMatch, "\'?\n"]
        in  firstError <> getCommandErrorString rest


{- | isSequentialSubsequence takes two lists and determines if the first List is
a subsequence of the second but the elements must be sequencetial unlike
isSubsequenceOf in Data.List
Uses Text.filter to see if there is a match
isSequentialSubsequence :: (Eq a) => [a] -> [a] -> Bool
-}
isSequentialSubsequence ∷ String → String → Bool
isSequentialSubsequence firstL secondL
    | null firstL = False
    | length firstL > length secondL = False
    | otherwise =
        let foundNumber = T.count (T.pack firstL) (T.pack secondL)
        in  foundNumber /= 0


{-# NOINLINE shuffleIO #-}


{- | shuffle Randomly shuffles a list
  /O(N)/
from https://wiki.haskell.org/Random_shuffle
-}
shuffleIO ∷ [a] → IO [a]
shuffleIO xs = do
    ar ← newArrayLocal n xs
    forM [1 .. n] $ \i → do
        j ← randomRIO (i, n)
        vi ← readArray ar i
        vj ← readArray ar j
        writeArray ar j vi
        pure vj
    where
        n = length xs
        newArrayLocal ∷ Int → [a] → IO (IOArray Int a)
        newArrayLocal nL = newListArray (1, nL)


{-# NOINLINE shuffleInt #-}


{- | shuffleInt takes a seed, number of replicates and a list of Ints and
repeately shuffles the order
-}
shuffleInt ∷ Int → Int → [Int] → [[Int]]
shuffleInt seed numReplicates inIntList
    | null inIntList = []
    | numReplicates < 1 = []
    | otherwise =
        let randList = take (length inIntList) $ randomIntList seed
            pairList = sortOn fst $ zip randList inIntList
            newList = snd <$> pairList
        in  newList : shuffleInt (seed + 1) (numReplicates - 1) inIntList


{-# NOINLINE randomList #-}


{- | randomList generates an infinite random list from a seed--no IO or ST monad
but needs a good seed--perhaps system tiem
can cast to to other types like :: [Int]
-}
randomList ∷ Int → [Double]
randomList seed = randoms (mkStdGen seed) ∷ [Double]


{-# NOINLINE randomIntList #-}


-- | randomIntList generates an infinite random list of Ints
randomIntList ∷ Int → [Int]
randomIntList seed = randoms (mkStdGen seed) ∷ [Int]


{-# NOINLINE permuteList #-}


-- | permuteList ranomzes list order with seed
permuteList ∷ Int → [a] → [a]
permuteList rSeed inList
    | null inList = []
    | length inList == 1 = inList
    | otherwise = fmap fst . sortOn snd . zip inList $ randomIntList rSeed


-- | takeRandom permutes a list and takes a number based on seed and number to take
takeRandom ∷ Int → Int → [a] → [a]
takeRandom rSeed number inList
    | null inList = []
    | number >= length inList = inList
    | otherwise = L.take number $ permuteList rSeed inList


-- | takeNth takes n elments (each nth) of a list of length m
takeNth ∷ Int → [a] → [a]
takeNth 0 = const []
takeNth n = \case
    [] → []
    x : _ | n == 1 → [x]
    inList →
        let len = length inList
        in  case n `compare` len of
                LT →
                    let (value, _) = divMod len n
                        indexList = [0 .. len - 1]
                        (_, remList) = unzip . zipWith divMod indexList $ L.replicate len value
                        (outList, _) = unzip . filter ((== 1) . snd) $ zip inList remList
                    in  take n outList
                _ → inList


{-# NOINLINE getRandomElement #-}


{- | getRandomElement returns the nth random element uniformly
at random
-}
getRandomElement ∷ Int → [a] → a
getRandomElement rVal = \case
    [] → error "Null list in getRandomElement"
    inList@(x : xs) → case xs of
        [] → x
        _ →
            let len = length inList
                idx = abs rVal `mod` len
            in  inList !! idx


{- | chooseElementAtRandomPair like chooseElementAtRandomWithDistribution
with a list of element fraction pairs as opposed to two lists
gets randValList on input and returns tail to avoid long number of
access of list later
-}
chooseElementAtRandomPair ∷ Double → [(a, Double)] → a
chooseElementAtRandomPair randVal elemDistList
    | null elemDistList = error "Null lists input to chooseElementAtRandomPair"
    | otherwise = getElementIntervalPair randVal 0.0 elemDistList


-- | getElementIntervalPair recursively checks if teh double input is in the current interval if not recurses
getElementIntervalPair ∷ Double → Double → [(a, Double)] → a
getElementIntervalPair doubleVal minVal = \case
    [] → error "Null list in getElementIntervalPair"
    (fstElem, fstDouble) : xs →
        let maxVal = minVal + fstDouble
        in  case xs of
                -- case where == 1.0
                [] → fstElem
                -- in interval
                _ | doubleVal >= minVal && doubleVal < maxVal → fstElem
                -- not in interval
                _ → getElementIntervalPair doubleVal maxVal xs


{-# NOINLINE chooseElementAtRandomWithDistribution #-}


{- | chooseElementAtRandomWithDistribution takes a seed, list of values,
and a list of Double frequencies as distrinbution returning
a single element at random based on the distribution
assumes ditribution values sum to 1
-}
chooseElementAtRandomWithDistribution ∷ (Show a) ⇒ Int → [a] → [Double] → a
chooseElementAtRandomWithDistribution rSeed elementList distributionList
    | length elementList /= length distributionList =
        error $
            unwords
                [ "chooseElementAtRandomWithDistribution: Unequal element and distribution lists:"
                , show (length elementList, elementList)
                , "versus"
                , show (length distributionList, distributionList)
                ]
    | null elementList = error "Null lists input to chooseElementAtRandomWithDistribution"
    | otherwise =
        -- generate uniform random Int
        let (randVal, _) = random (mkStdGen rSeed) ∷ (Double, StdGen)
        in  getElementInterval randVal 0.0 elementList distributionList


-- | getElementInterval recursively checks if teh double input is in the current interval if not recurses
getElementInterval ∷ Double → Double → [a] → [Double] → a
getElementInterval doubleVal minVal elementList = \case
    [] → error "Null list in call: getElementInterval _ _ _ []"
    x : xs → case elementList of
        [] → error "Null list in call: getElementInterval _ _ [] _"
        y : ys →
            let maxVal = minVal + x
            in  if doubleVal >= minVal && doubleVal < maxVal
                    then y
                    else getElementInterval doubleVal maxVal ys xs


{- | selectListCostPairs is general to list of (a, Double)
but here used for graph sorting and selecting)takes a pair of graph representation (such as String or fgl graph), and
a Double cost and returns the whole of number of 'best', 'unique' or  'random' cost
need an Eq function such as '==' for Strings or equal for fgl
assumes options are all lower case
options are pairs of String and number for number or graphs to keeep, if number is set to (-1) then all are kept
if the numToKeep to return graphs is lower than number of graphs, the "best" number are returned
except for random.
-}
selectListCostPairs ∷ ∀ a. (a → a → Bool) → [(a, Double)] → [String] → Int → Int → [(a, Double)]
selectListCostPairs compFun pairList optionList numToKeep seed =
    let compFunPair ∷ ∀ b c. (a, b) → (a, c) → Bool
        compFunPair x = compFun (fst x) . fst

        definePass ∷ ∀ e. String → ([e] → [e]) → [e] → [e]
        definePass token f
            | token `elem` optionList = f
            | otherwise = id

        pass1 ∷ ∀ d. [(a, d)] → [(a, d)]
        pass1 = definePass "unique" $ L.nubBy compFunPair

        pass2 ∷ ∀ d. (Ord d) ⇒ [(a, d)] → [(a, d)]
        pass2 = definePass "best" $ sortOn (Down . snd)

        pass3 ∷ ∀ d. [(a, d)] → [(a, d)]
        pass3 =
            definePass "random" $
                let randList = randomList seed
                in  fmap snd . sortOn fst . zip randList

        transform = take numToKeep . pass3 . pass2 . pass1
    in  transform pairList


-- | getSystemTimeSeconds gets teh syste time and returns IO Int
getSystemTimeSeconds ∷ IO Int
getSystemTimeSeconds = do
    systemTime ← getCurrentTime
    let timeD = (round $ utcTimeToPOSIXSeconds systemTime) ∷ Int
    pure timeD


{-# NOINLINE getSystemTimeNDT #-}


-- | getSystemTimeNDT gets the syste time and returns IO NominalDiffTime
getSystemTimeNDT ∷ IO NominalDiffTime
getSystemTimeNDT = do
    systemTime ← getCurrentTime
    let !timeD = utcTimeToPOSIXSeconds systemTime
    pure timeD


{-# NOINLINE getSystemTimeNDTUnsafe #-}


-- | getSystemTimeNDTUnsafe gets the system time and returns IO NominalDiffTime
getSystemTimeNDTUnsafe ∷ NominalDiffTime
getSystemTimeNDTUnsafe = unsafePerformIO getSystemTimeNDT


{-# NOINLINE getSystemTimeSecondsUnsafe #-}


{- | getSystemTimeSecondsUnsafe gets the system time and returns Int via unsafePerformIO
without the NOINLINE the function would probbaly be comverted to a
constant which would be "safe" and OK as a random seed or if only called once
-}
getSystemTimeSecondsUnsafe ∷ Int
getSystemTimeSecondsUnsafe = unsafePerformIO $ force <$> getSystemTimeSeconds


-- | stringToInt converts a String to an Int
stringToInt ∷ String → String → Int
stringToInt fileName inStr = case readMaybe inStr of
    Just v → v
    Nothing →
        errorWithoutStackTrace $
            "\n\n'Read' 'tcm' format error non-Integer value " <> inStr <> " in " <> fileName


-- | stringToDouble converts a String to a Double
stringToDouble ∷ String → String → Double
stringToDouble fileName inStr = case readMaybe inStr of
    Just v → v
    Nothing →
        errorWithoutStackTrace $
            "\n\n'Read' 'tcm' format error non-Double value " <> inStr <> " in " <> fileName


-- | makeIndexPairs takes n and creates upper triangular matrix pairs (0,m)
makeIndexPairs ∷ Bool → Int → Int → Int → Int → [(Int, Int)]
makeIndexPairs doDiagValues numI numJ indexI indexJ
    | indexI == numI = []
    | indexJ == numJ = makeIndexPairs doDiagValues numI numJ (indexI + 1) 0
    | doDiagValues && indexI == indexJ = (indexI, indexJ) : makeIndexPairs doDiagValues numI numJ indexI (indexJ + 1)
    | indexI < indexJ = (indexI, indexJ) : makeIndexPairs doDiagValues numI numJ indexI (indexJ + 1)
    | otherwise = makeIndexPairs doDiagValues numI numJ indexI (indexJ + 1)


{- | stripString  removes leading and trailing spaces from String
akin to Text 'strip'
-}
stripString ∷ String → String
stripString =
    let trim = reverse . dropWhile (== ' ')
    in  trim . trim


{- | replaceVal replaces first value with second value e.g.  carriage return '\r' with line newlinme '\n'
call with [] accumulator
-}
replaceVal ∷ (Eq a) ⇒ a → a → [a] → [a] → [a]
replaceVal target replacement inList curList = case inList of
    [] → reverse curList
    x : xs →
        let newHead
                | x == target = replacement
                | otherwise = x
        in  replaceVal target replacement xs $ newHead : curList


-- | cartProd takes two lists and retuns cartesian product as list of pairs
cartProd ∷ [a] → [b] → [(a, b)]
cartProd xs ys = [(x, y) | x ← xs, y ← ys]


-- | cartProdPair takes a pair of lists and retuns cartesian product as list of pairs
cartProdPair ∷ ([a], [b]) → [(a, b)]
cartProdPair (xs, ys) = [(x, y) | x ← xs, y ← ys]


{- | isCompatible takes a bit vector and a list of bit vectors
and returns True if the fist bit vector is compatible will all in the list
-}
isBVCompatible ∷ BV.BitVector → [BV.BitVector] → Bool
isBVCompatible _ [] = True
isBVCompatible inBV (b : bs)
    | bvVal == inBV = isBVCompatible inBV bs
    | bvVal == b = isBVCompatible inBV bs
    | otherwise = False
    where
        bvVal = inBV .&. b


{- |
textMatchWildcards takes two Text's first may have wildcards and second without
return True if they match, False otherwise.

TODO: use the 'Glob' library
-}
textMatchWildcards ∷ TL.Text → TL.Text → Bool
textMatchWildcards straightText wildText
    | TL.null wildText && TL.null straightText = True
    | TL.null wildText = False
    | (TL.head wildText == '*') && (TL.length wildText == 1) = True
    | TL.null straightText = False
    | TL.head wildText == '*'
        && TL.length (TL.dropWhile (== '*') wildText) > 0
        && TL.null straightText =
        False
    | TL.head wildText == '?'
        || TL.head wildText == TL.head straightText =
        textMatchWildcards (TL.tail straightText) (TL.tail wildText)
    | TL.head wildText == '*' =
        textMatchWildcards (TL.tail straightText) wildText || textMatchWildcards straightText (TL.tail wildText)
    | otherwise = False


-- | elemWildards checks if a Text matches (without wildcards) at least one element of a List of Wildcard Text
elemWildcards ∷ TL.Text → [TL.Text] → Bool
elemWildcards _ [] = False
elemWildcards straightText (t : ts)
    | textMatchWildcards straightText t = True
    | otherwise = elemWildcards straightText ts


-- | notElemWildcards checks if a Text matches (without wildcards) no elements of a List of Wildcard Text
notElemWildcards ∷ TL.Text → [TL.Text] → Bool
notElemWildcards = (not .) . elemWildcards


{- | getListPairs takes a list and returns all unique pairs of elements
order is (first found in list, second found in list)
-}
getListPairs ∷ [a] → [(a, a)]
getListPairs = \case
    [] → []
    x : xs → zip (replicate (length xs) x) xs <> getListPairs xs
