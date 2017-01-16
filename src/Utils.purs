{-
  (C) 2017 David Lettier
  lettier.com
-}

module Utils where

import Prelude

import Global (readFloat, isNaN, isFinite)

import Data.Generic (gShow)
import Data.Array (length, head)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Foldable (foldl)
import Data.Int (toNumber)

firstOrSecondValues :: (Array Number -> Maybe Number) -> Array (Array Number) -> Array Number
firstOrSecondValues f [] = []
firstOrSecondValues f es = foldl innerFold [] (map f es)
  where
    innerFold acc (Just e) = acc <> [e]
    innerFold acc Nothing  = acc

first :: Array Number -> Maybe Number
first [x, y] = Just x
first _      = Nothing

second :: Array Number -> Maybe Number
second [x, y] = Just y
second _      = Nothing

lengthNum :: forall a. Array a -> Number
lengthNum [] = 0.0
lengthNum xs = (toNumber <<< length) xs

arrayNumberHandler :: (Number -> Number -> Number) -> Array Number -> Number
arrayNumberHandler f [x, y] = f x y
arrayNumberHandler _ _      = 0.0

stringToMaybeNumber :: String -> Maybe Number
stringToMaybeNumber s = maybeNumber
  where
    float :: Number
    float = readFloat s
    maybeNumber :: Maybe Number
    maybeNumber = if isNaN float then Nothing else Just float

maybeNumberToString :: Maybe Number -> String
maybeNumberToString Nothing  = ""
maybeNumberToString (Just a) = gShow a

isInfinity :: Number -> Boolean
isInfinity = not <<< isFinite

arrayMinOrMax :: forall a. (Ord a) => (a -> a -> a) -> a -> Array a -> a
arrayMinOrMax _ default []     = default
arrayMinOrMax f _       [h]    = h
arrayMinOrMax f default values = foldl (\ acc value -> f acc value) (fromMaybe default (head values)) values

numberInvalid :: Number -> Boolean
numberInvalid n = isNaN n || isInfinity n

containsNothing :: forall a. Array (Maybe a) -> Boolean
containsNothing xs = foldl (\ acc x -> acc || isNothing x) false xs
