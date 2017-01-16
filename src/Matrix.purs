{-
  (C) 2017 David Lettier
  lettier.com
-}

module Matrix where

import Prelude

import Data.Ord (abs)
import Data.Foldable (foldl, sum)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (head, length, null, range, slice, tail, updateAt, zip, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing)

type Vector = Array Number
type Matrix = Array Vector

invertMatrix :: Matrix -> Tuple (Either String Matrix) Matrix
invertMatrix matrix
  | null matrix                 = Tuple (Left "Empty matrix")                (identityMatrix (length matrix))
  | not $ isSquareMatrix matrix = Tuple (Left "Not a square matrix")         (identityMatrix (length matrix))
  | containsZeroRowOrCol matrix = Tuple (Left "Contains zero row or column") (identityMatrix (length matrix))
  | otherwise = result
  where
    matrixSize = length matrix
    identity   = identityMatrix matrixSize
    result :: Tuple (Either String Matrix) Matrix
    result = foldl folder (Tuple (Right matrix) identity) (matrixToRange matrix)
    folder :: Tuple (Either String Matrix) Matrix -> Int -> Tuple (Either String Matrix) Matrix
    folder t@(Tuple acc@(Left  _) _) _        = t
    folder t@(Tuple acc@(Right a) b) diagonal =
      if containsZeroRowOrCol a || null a || isNothing maybeDivisorA || divisorA == 0.0
        then (Tuple (Left "Cannot invert") b)
        else (Tuple (Right clearedA) clearedB)
      where
        swapped       = swapWithValidRow (Tuple a b) diagonal
        swappedA      = fst swapped
        swappedB      = snd swapped
        divisorRowA   = fromMaybe [] (swappedA !! diagonal)
        maybeDivisorA = divisorRowA !! diagonal
        divisorA      = fromMaybe defaultMatrixValue maybeDivisorA
        multiplierA   = 1.0 / divisorA
        multipliedA   = multiplyRow swappedA diagonal multiplierA
        multipliedB   = multiplyRow swappedB diagonal multiplierA
        cleared       = clearColumnExceptPivot (Tuple multipliedA multipliedB) (Tuple diagonal diagonal)
        clearedA      = fst cleared
        clearedB      = snd cleared

multiplyMatrices :: Matrix -> Matrix -> Maybe Matrix
multiplyMatrices _  [] = Nothing
multiplyMatrices [] _  = Nothing
multiplyMatrices aMat bMat = if aMatColNum /= bMatRowNum then Nothing else Just cMat
  where
    aMatColNum :: Int
    aMatColNum = foldl (\ acc r -> length r) 0 aMat
    bMatRowNum :: Int
    bMatRowNum = length bMat
    tBMat :: Matrix
    tBMat = transpose bMat
    cMat :: Matrix
    cMat = map (\ r ->
        map (\ c ->
            sum $ map (\ (Tuple a b) ->
                a * b
              ) (zip r c)
          ) tBMat
      ) aMat

defaultMatrix :: Int -> Matrix
defaultMatrix size = buildMatrix size value
  where
    value _ _ = defaultMatrixValue

identityMatrix :: Int -> Matrix
identityMatrix size = buildMatrix size value
  where
    value row col | row == col = 1.0
                  | otherwise  = 0.0

buildMatrix :: Int -> (Int -> Int -> Number) -> Matrix
buildMatrix 0    _ = []
buildMatrix size f = map (\ row -> map (\ col -> f row col) matrixRange) matrixRange
  where
    matrixRange = range 0 (size - 1)

matrixSizeValid :: Int -> Boolean
matrixSizeValid size = size <= maxMatrixSize && size >= minMatrixSize

maybeMatrixValue :: Matrix -> Int -> Int -> Maybe Number
maybeMatrixValue matrix row col = case matrix !! row of
                                    Nothing -> Nothing
                                    Just x  -> x !! col

matrixRow :: Matrix -> Int -> Vector
matrixRow matrix i = fromMaybe [] (matrix !! i)

firstValidRow :: Matrix -> Int -> Int -> Maybe Int
firstValidRow []     _            _     = Nothing
firstValidRow matrix atRowOrBelow inCol = (foldl folder { index: Nothing, value: Nothing} tuples).index
  where
    matrixLength = length matrix
    lastRow = matrixLength - 1
    rowRange = range atRowOrBelow lastRow
    column = slice atRowOrBelow matrixLength (matrixRow (transpose matrix) inCol)
    tuples = zip rowRange column
    folder :: { index :: Maybe Int, value :: Maybe Number } -> Tuple Int Number -> { index :: Maybe Int, value :: Maybe Number }
    folder { index: Nothing, value: Nothing} (Tuple a b) = { index: (Just a), value: (Just b) }
    folder r@{ index: (Just x), value: (Just y)} (Tuple a b) =
      if abs y == 1.0
        then r
        else
          if abs b >= abs y || abs b == 1.0
            then { index: (Just a), value: (Just b) }
            else r
    folder _ _ = { index: Nothing, value: Nothing }

swapWithValidRow :: Tuple Matrix Matrix -> Int -> Tuple Matrix Matrix
swapWithValidRow t@(Tuple a b) diagonal = Tuple a' b'
  where
    row = firstValidRow a diagonal diagonal
    yesSwap = case row of
                Nothing -> false
                Just r  -> r /= diagonal
    swapIfYes m =
      if yesSwap
         then swapRows m diagonal (fromMaybe diagonal row)
         else m
    a' = swapIfYes a
    b' = swapIfYes b

transpose :: Matrix -> Matrix
transpose []     = []
transpose matrix = transpose' matrix []
  where
    transpose' :: Matrix -> Matrix -> Matrix
    transpose' old new = if arrayHasNothing maybeHeads then new else transpose' tails (new <> [heads])
      where
        maybeHeads = map head old
        maybeTails = map tail old
        heads = map (fromMaybe 0.0) maybeHeads
        tails = map (fromMaybe []) maybeTails
        arrayHasNothing :: forall a. Array (Maybe a) -> Boolean
        arrayHasNothing array = case array !! 0 of
                                  Nothing  -> true
                                  (Just x) -> isNothing x

swapRows :: Matrix -> Int -> Int -> Matrix
swapRows []     _ _ = []
swapRows matrix a b = fromMaybe [] (updateRow b rowA (updateRow a rowB (Just matrix)))
  where
    rowA :: Maybe Vector
    rowA = matrix !! a
    rowB :: Maybe Vector
    rowB = matrix !! b
    updateRow :: Int -> Maybe Vector -> Maybe Matrix -> Maybe Matrix
    updateRow i (Just row) (Just matrix') = updateAt i row matrix'
    updateRow _ _ _ = Nothing

multiplyRow :: Matrix -> Int -> Number -> Matrix
multiplyRow []     _   _          = []
multiplyRow matrix row multiplier = map (\ row' ->
    if row == row'
      then map (\ value -> value * multiplier) (matrixRow matrix row')
      else matrixRow matrix row'
  ) (matrixToRange matrix)

clearValue :: Tuple Matrix Matrix -> Tuple Int Int -> Tuple Int Int -> Tuple Matrix Matrix
clearValue (Tuple aMat bMat) pivot@(Tuple pRow pCol) target@(Tuple tRow tCol) = Tuple aMat' bMat'
  where
    pivotRowA :: Vector
    pivotRowA         = matrixRow aMat pRow
    targetRowA :: Vector
    targetRowA        = matrixRow aMat tRow
    pivotValueA :: Number
    pivotValueA       = rowValue pivotRowA pCol
    targetValueA :: Number
    targetValueA      = rowValue targetRowA tCol
    aMat' :: Matrix
    aMat'             = multiplyAndSubtractRows aMat tRow pRow pivotValueA targetValueA
    bMat' :: Matrix
    bMat'             = multiplyAndSubtractRows bMat tRow pRow pivotValueA targetValueA
    rowValue :: Vector -> Int -> Number
    rowValue row col  = fromMaybe defaultMatrixValue (row  !! col)
    multiplyAndSubtractRows :: Matrix -> Int -> Int -> Number -> Number -> Matrix
    multiplyAndSubtractRows
      matrix
      leftSideRow
      rightSideRow
      leftMultiplier
      rightMultiplier
      = map (\ row -> if row == leftSideRow
                        then leftRowValues'
                        else matrixRow matrix row
        ) (matrixToRange matrix)
      where
        leftRowValues  = matrixRow matrix leftSideRow
        rightRowValues = matrixRow matrix rightSideRow
        leftRowValues' = map (\ (Tuple l r) ->
            leftMultiplier * l - rightMultiplier * r
          ) (zip leftRowValues rightRowValues)

clearColumnExceptPivot :: Tuple Matrix Matrix -> Tuple Int Int -> Tuple Matrix Matrix
clearColumnExceptPivot t@(Tuple aMat bMat) pivot@(Tuple pRow pCol) = t'
  where
    t' :: Tuple Matrix Matrix
    t' = foldl folder t (matrixToRange aMat)
    folder :: Tuple Matrix Matrix -> Int -> Tuple Matrix Matrix
    folder acc@(Tuple aMat' bMat') row =
      if row == pRow
        then acc
        else clearValue acc pivot (Tuple row pCol)

containsZeroRowOrCol :: Matrix -> Boolean
containsZeroRowOrCol matrix = containsZeroRow matrix || containsZeroCol matrix

containsZeroCol :: Matrix -> Boolean
containsZeroCol = containsZeroRow <<< transpose

containsZeroRow :: Matrix -> Boolean
containsZeroRow = foldl (\ acc row -> acc || foldl (\ acc' value -> acc' && value == 0.0) true row) false

isSquareMatrix :: Matrix -> Boolean
isSquareMatrix matrix = foldl (\ acc row -> acc && length row == matrixLength) true matrix
  where
    matrixLength = length matrix

matrixToRange :: Matrix -> Array Int
matrixToRange []     = []
matrixToRange matrix = (range 0 (length matrix - 1))

maxMatrixSize :: Int
maxMatrixSize = 10

minMatrixSize :: Int
minMatrixSize = 2

defaultMatrixValue :: Number
defaultMatrixValue = 0.0
