{-
  (C) 2017 David Lettier
  lettier.com
-}

module LinearRegression (
      runLinearRegressionWithUnscaled
    , calculatePressStatistic
  ) where

import Prelude

import Math (pow)

import Data.Ord (abs)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Either (Either(..))
import Data.Foldable (sum, foldl)
import Data.Array (zip, (!!), range, length)
import Data.Tuple (Tuple(..))

import Utils (lengthNum, containsNothing)

import Matrix (Vector, Matrix, transpose, multiplyMatrices, invertMatrix)

runLinearRegressionWithUnscaled :: Int -> Number -> Number -> Vector -> Vector -> Matrix -> Vector
runLinearRegressionWithUnscaled
  maxIterations
  maxCost
  learningRate
  coefficients
  regressands
  unscaledDesignMatrix
  = if containsNothing unscaledUpdatedCoefficents then [] else map (fromMaybe 0.0) unscaledUpdatedCoefficents
  where
    transposedUnscaledDesignMatrix = transpose unscaledDesignMatrix
    maybeMeans = map mean transposedUnscaledDesignMatrix
    maybeStandardDeviations = map (\ (Tuple m v) ->
        standardDeviation m v
      ) (zip maybeMeans transposedUnscaledDesignMatrix)
    scaledTransposedDesignMatrix = scaleTransposedDesignMatrix maybeMeans maybeStandardDeviations transposedUnscaledDesignMatrix
    scaledDesignMatrix = transpose scaledTransposedDesignMatrix
    scaledUpdatedCoefficents =
      runLinearRegressionWithScaled
        0
        maxIterations
        maxCost
        learningRate
        coefficients
        regressands
        scaledDesignMatrix
    unscaledUpdatedCoefficents = unscaleCoefficients maybeMeans maybeStandardDeviations scaledUpdatedCoefficents

runLinearRegressionWithScaled :: Int -> Int -> Number -> Number -> Vector -> Vector -> Matrix -> Vector
runLinearRegressionWithScaled
  currentIteration
  maxIterations
  maxCost
  learningRate
  coefficients
  regressands
  scaledDesignMatrix
  =
    if
      currentCalculatedCost <= maxCost ||
      currentIteration >= maxIterations ||
      abs (currentCalculatedCost - previousCalculatedCost) == 0.0
      then coefficients'
      else
        runLinearRegressionWithScaled
          (currentIteration + 1)
          maxIterations
          maxCost
          learningRate'
          coefficients'
          regressands
          scaledDesignMatrix
  where
    updatedCoefficients = updateCoefficients learningRate coefficients scaledDesignMatrix regressands
    previousCalculatedCost = calculateCost coefficients        scaledDesignMatrix regressands
    currentCalculatedCost  = calculateCost updatedCoefficients scaledDesignMatrix regressands
    -- Bold Driver - http://www.willamette.edu/~gorr/classes/cs449/momrate.html
    coefficients' = if previousCalculatedCost < currentCalculatedCost then coefficients else updatedCoefficients
    learningRate' =
      if previousCalculatedCost < currentCalculatedCost
        then learningRate - (learningRate * 0.5)
        else learningRate + (learningRate * 0.5)

hypothesis :: Vector -> Vector -> Number
hypothesis coefficients regressors = sum $ map (\ (Tuple c r) -> c * r) tuples
  where
    tuples = zip coefficients ([1.0] <> regressors)

calculateCost :: Vector -> Matrix -> Vector -> Number
calculateCost _            _            []          = 0.0
calculateCost coefficients designMatrix regressands = sum errors / size
  where
    size = lengthNum regressands
    regressands' = map (hypothesis coefficients) designMatrix
    errors = map (\ (Tuple y' y) -> pow (y' - y) 2.0) (zip regressands' regressands)

partialDerivative :: Int -> Vector -> Matrix -> Vector -> Number
partialDerivative _ []           _            _           = 0.0
partialDerivative _ _            []           _           = 0.0
partialDerivative _ _            _            []          = 0.0
partialDerivative i coefficients designMatrix regressands = if i < 0 then 0.0 else result
  where
    size = lengthNum regressands
    tuples = zip regressands designMatrix
    result = (1.0 / size) * (sum (map mapper tuples))
    mapper :: Tuple Number Vector -> Number
    mapper (Tuple _ []) = 0.0
    mapper (Tuple y regressors) = ((hypothesis coefficients regressors) - y) * xj
      where
        xj = if i == 0 then 1.0 else (fromMaybe 0.0 (regressors !! (i - 1)))

updateCoefficients :: Number -> Vector -> Matrix -> Vector -> Vector
updateCoefficients learningRate coefficients designMatrix regressands = result
  where
    paritalDerivativeMapper i = partialDerivative i coefficients designMatrix regressands
    gradient = map paritalDerivativeMapper (range 0 (length coefficients - 1))
    result = map (\ (Tuple o g) -> o - (learningRate * g)) (zip coefficients gradient)

scaleTransposedDesignMatrix :: Array (Maybe Number) -> Array (Maybe Number) -> Matrix -> Matrix
scaleTransposedDesignMatrix []         _                       _                      = []
scaleTransposedDesignMatrix _          []                      _                      = []
scaleTransposedDesignMatrix _          _                       []                     = []
scaleTransposedDesignMatrix maybeMeans maybeStandardDeviations transposedDesignMatrix = map scaleVector range'
  where
    scaleValue' :: Maybe Number -> Maybe Number -> Number -> Number
    scaleValue' mm ms e = fromMaybe e (scaleValue mm ms e)
    scaleVector :: Int -> Vector
    scaleVector i = map (scaleValue' (getMaybeMean i) (getMaybeStandardDeviation i)) (getRow i)
    range' :: Array Int
    range' = range 0 (length transposedDesignMatrix - 1)
    getMaybeMean :: Int -> Maybe Number
    getMaybeMean i = fromMaybe Nothing (maybeMeans !! i)
    getMaybeStandardDeviation :: Int -> Maybe Number
    getMaybeStandardDeviation i = fromMaybe Nothing (maybeStandardDeviations !! i)
    getRow :: Int -> Vector
    getRow i = fromMaybe [] (transposedDesignMatrix !! i)

scaleValue :: Maybe Number -> Maybe Number -> Number -> Maybe Number
scaleValue _            (Just 0.0)                unscaledValue = Nothing
scaleValue (Just mean') (Just standardDeviation') unscaledValue = Just ((unscaledValue - mean') / standardDeviation')
scaleValue _            _                         _             = Nothing

unscaleCoefficients :: Array (Maybe Number) -> Array (Maybe Number) -> Vector -> Array (Maybe Number)
unscaleCoefficients [] _  _  = []
unscaleCoefficients _  [] _  = []
unscaleCoefficients _  _  [] = []
unscaleCoefficients
  maybeMeans
  maybeStandardDeviations
  coefficients
  =
  if
    length maybeMeans /= length maybeStandardDeviations ||
    length maybeMeans /= (length coefficients - 1) ||
    containsNothing maybeMeans ||
    containsNothing maybeStandardDeviations
    then []
    else map (\ i -> extractAndApply i unscale) (range 0 (length coefficients - 1))
  where
    maybeMeans' = [Nothing] <> maybeMeans
    maybeStandardDeviations' = [Nothing] <> maybeStandardDeviations
    extractAndApply :: Int -> (Int -> Number -> Number -> Number -> Maybe Number) -> Maybe Number
    extractAndApply index f =
      if
        index < 0 ||
        index >= length maybeMeans' ||
        index >= length maybeStandardDeviations' ||
        index >= length coefficients
          then Nothing
          else f index (fromMaybe 0.0 maybeMean) (fromMaybe 0.0 maybeStandardDeviation) coefficient
      where
        maybeMean :: Maybe Number
        maybeMean = fromMaybe Nothing (maybeMeans' !! index)
        maybeStandardDeviation :: Maybe Number
        maybeStandardDeviation = fromMaybe Nothing (maybeStandardDeviations' !! index)
        coefficient :: Number
        coefficient = fromMaybe 0.0 (coefficients !! index)
    unscale :: Int -> Number -> Number -> Number -> Maybe Number
    unscale 0 _ _ coefficient =
      if containsNothing summands
        then Nothing
        else Just (coefficient - summation)
      where
        summation :: Number
        summation = foldl (\ acc x -> acc + (fromMaybe 0.0 x)) 0.0 summands
        summands :: Array (Maybe Number)
        summands = map (\ i ->
            extractAndApply i (\ _ m s c ->
                if s == 0.0 then Nothing else Just (c * (m / s))
              )
          ) (range 1 (length coefficients - 1))
    unscale _     _ 0.0 _           = Nothing
    unscale index _ std coefficient = if index < 0 then Nothing else Just (coefficient / std)

calculatePressStatistic :: Vector -> Vector -> Matrix -> Maybe Number
calculatePressStatistic []          _            _            = Nothing
calculatePressStatistic _           []           _            = Nothing
calculatePressStatistic _           _            []           = Nothing
calculatePressStatistic regressands coefficients designMatrix = maybeSummation
  where
    predictions :: Vector
    predictions = map (\ regressors -> hypothesis coefficients regressors) designMatrix
    residuals :: Vector
    residuals = map (\ (Tuple y y') -> y - y') (zip regressands predictions)
    hatMatrix :: Matrix
    hatMatrix = fromMaybe [] (calculateHatMatrix designMatrix)
    folder :: Maybe Number -> Int -> Maybe Number
    folder Nothing  _ = Nothing
    folder (Just acc) i =
      if
        isNothing maybeResidual ||
        isNothing maybeTerm ||
        1.0 - term == 0.0
          then Nothing
          else Just (acc + (pow (residual / (1.0 - term)) 2.0))
      where
        maybeResidual :: Maybe Number
        maybeResidual = residuals !! i
        residual :: Number
        residual = fromMaybe 0.0 maybeResidual
        row :: Vector
        row = fromMaybe [] (hatMatrix !! i)
        maybeTerm :: Maybe Number
        maybeTerm = row !! i
        term :: Number
        term = fromMaybe 0.0 maybeTerm
    maybeSummation :: Maybe Number
    maybeSummation = foldl folder (Just 0.0) (range 0 (length regressands - 1))

calculateHatMatrix :: Matrix -> Maybe Matrix
calculateHatMatrix [] = Nothing
calculateHatMatrix designMatrix = xxTxIxT
  where
    xT :: Matrix
    xT = transpose designMatrix
    xTx :: Matrix
    xTx = fromMaybe [] (multiplyMatrices xT designMatrix)
    xTxI :: Matrix
    xTxI =
      case invertMatrix xTx of
        (Tuple (Right i) m) -> m
        _                   -> []
    xxTxI :: Matrix
    xxTxI = fromMaybe [] (multiplyMatrices designMatrix xTxI)
    xxTxIxT :: Maybe Matrix
    xxTxIxT = multiplyMatrices xxTxI xT

standardDeviation :: Maybe Number -> Vector -> Maybe Number
standardDeviation _            [] = Nothing
standardDeviation (Just mean') es = Just (pow base 0.5)
  where
    oneOverLength = 1.0 / lengthNum es
    summation     = sum $ map (\ e -> pow (e - mean') 2.0) es
    base          = oneOverLength * summation
standardDeviation _            _  = Nothing

mean :: Vector -> Maybe Number
mean [] = Nothing
mean es = Just (sum es / lengthNum es)
