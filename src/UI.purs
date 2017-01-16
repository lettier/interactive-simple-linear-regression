{-
  (C) 2017 David Lettier
  lettier.com
-}

module UI where

import Prelude

import Data.Generic (gShow)
import Data.Foldable (foldr)
import Data.Array (drop, (:), length, head, last, range, filter)
import Data.List.Lazy (replicateM, (!!))
import Data.Maybe (Maybe(..), isNothing, fromMaybe, isJust)

import Control.Monad.Eff.Random (RANDOM, randomRange)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Aff.Console (CONSOLE, log)

import Halogen as H
import Halogen.HTML.Core (className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Indexed as HH

import LinearRegression (
      runLinearRegressionWithUnscaled
    , calculatePressStatistic
  )

import Plot (PLOT, PlotData, makePlot)

import Utils (maybeNumberToString, stringToMaybeNumber, arrayMinOrMax)

type Effects eff = H.HalogenEffects (plot :: PLOT, console :: CONSOLE, random :: RANDOM | eff)

type Point = { id :: Int, x :: Maybe Number, y :: Maybe Number }

data Query a =
  PushPoint a |
  PopPoint a |
  RemovePoint Int a |
  RandomPoints a |
  UpdatePointCoordinate Int String String a |
  RunLinearRegression a

type State = {
      nextId :: Int
    , points :: Array Point
    , yIntercept :: Maybe Number
    , slope :: Maybe Number
    , pressStatistic :: Maybe Number
    , running :: Boolean
  }

ui :: forall eff. H.Component State Query (Aff (Effects eff))
ui = H.component { render, eval }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_ [
            HH.div_ [
                  HH.div_ [
                        HH.b_ [
                            HH.text "Status: "
                          ]
                      , HH.text
                          if state.running
                            then "Running"
                            else if hasValidPoints state then "Press run" else "Add points"
                    ]
                  , HH.div_ [
                          HH.b_ [
                              HH.text "Y-Intercept: "
                            ]
                        , HH.text
                          if isJust state.yIntercept && hasValidPoints state
                            then "" <> (maybeNumberToString state.yIntercept)
                            else "?"
                      ]
                  , HH.div_ [
                          HH.b_ [
                              HH.text "Slope: "
                            ]
                        , HH.text
                          if isJust state.slope && hasValidPoints state
                            then "" <> (maybeNumberToString state.slope)
                            else "?"
                      ]
                  , HH.div_ [
                          HH.b_ [
                              HH.text "PRESS Statistic: "
                            ]
                        , HH.text
                          if isJust state.pressStatistic && hasValidPoints state
                            then "" <> (maybeNumberToString state.pressStatistic)
                            else "?"
                      ]
              ]
            , HH.div_ [
                HH.button [
                      HE.onClick (HE.input_ PushPoint)
                  ] [
                      HH.text "Push Point"
                  ]
              , HH.button [
                      HE.onClick (HE.input_ PopPoint)
                  ] [
                      HH.text "Pop Point"
                  ]
              , HH.button [
                      HE.onClick (HE.input_ RandomPoints)
                    , HP.class_ (className "randomPointsButton")
                  ] [
                      HH.text "Random Points"
                  ]
              , HH.button [
                      HE.onClick (HE.input_ RunLinearRegression)
                    , HP.class_ (className "runButton")
                  ] [
                      HH.text "Run"
                  ]
              , HH.div_ (
                    map (\ point ->
                        HH.li_ [
                              HH.input [
                                    HP.value (maybeNumberToString point.x)
                                  , HP.placeholder "Input the X Coordinate"
                                  , HE.onValueChange (HE.input (UpdatePointCoordinate point.id "x"))
                                  , HP.disabled state.running
                                ]
                            , HH.input [
                                    HP.value (maybeNumberToString point.y)
                                  , HP.placeholder "Input the Y Coordinate"
                                  , HE.onValueChange (HE.input (UpdatePointCoordinate point.id "y"))
                                  , HP.disabled state.running
                                ]
                            , HH.button [
                                    HE.onClick (HE.input_ (RemovePoint point.id))
                                  , HP.class_ (className "removeButton")
                                ] [
                                    HH.text "X"
                                ]
                          ]
                      )
                      state.points
                  )
            ]
        ]
    eval :: Query ~> H.ComponentDSL State Query (Aff (Effects eff))
    eval (PushPoint next) = do
      H.modify (\ state -> state {
              nextId = state.nextId + 1
            , points = newPoint state.nextId : state.points
            , yIntercept = Nothing
            , slope = Nothing
            , pressStatistic = Nothing
          }
        )
      pure next
    eval (PopPoint next) = do
      H.modify (\ state -> state {
              points = drop 1 state.points
            , yIntercept = Nothing
            , slope = Nothing
            , pressStatistic = Nothing
          }
        )
      currentState <- H.get
      _ <- H.fromAff (makePlot (makePlotDataFromState currentState))
      pure next
    eval (RemovePoint id next) = do
      H.modify (\ state -> state {
              points = foldr (\ point acc ->
                  if point.id == id then acc else point : acc
                ) [] state.points
            , yIntercept = Nothing
            , slope = Nothing
            , pressStatistic = Nothing
          }
        )
      currentState <- H.get
      _ <- H.fromAff (makePlot (makePlotDataFromState currentState))
      pure next
    eval (RandomPoints next) = do
      let numberOfPoints = 10
      xs <- H.fromEff (replicateM numberOfPoints (randomRange (-10.0) 10.0))
      ys <- H.fromEff (replicateM numberOfPoints (randomRange (-10.0) 10.0))
      H.modify (\ state -> state {
              points = map (\ i -> {
                      id: i
                    , x: xs !! i
                    , y: ys !! i
                  }
                ) (range 0 (numberOfPoints - 1))
            , yIntercept = Nothing
            , slope = Nothing
          }
        )
      currentState <- H.get
      _ <- H.fromAff (makePlot (makePlotDataFromState currentState))
      pure next
    eval (UpdatePointCoordinate id key value next) = do
      H.modify (\ state -> state {
              points = foldr (\ point acc ->
                  (if point.id == id then updatePointCoordinateFromString point key value else point) : acc
                ) [] state.points
            , yIntercept = Nothing
            , slope = Nothing
            , pressStatistic = Nothing
          }
        )
      currentState <- H.get
      _ <- H.fromAff (makePlot (makePlotDataFromState currentState))
      pure next
    eval (RunLinearRegression next) = do
      H.modify (\ state ->
          state {
                yIntercept = Nothing
              , slope = Nothing
              , pressStatistic = Nothing
              , running = true
            }
        )
      currentState <- H.get
      let linearRegressionData = dataForLinearRegressionFromState currentState
      let result =
                    runLinearRegressionWithUnscaled
                      linearRegressionData.maxIterations
                      linearRegressionData.maxCost
                      linearRegressionData.learningRate
                      linearRegressionData.coefficients
                      linearRegressionData.regressands
                      linearRegressionData.designMatrix
      let pressStatistic =
                            calculatePressStatistic
                              linearRegressionData.regressands
                              result
                              linearRegressionData.designMatrix
      log' (gShow result)
      H.modify (\ state ->
          state {
                yIntercept = head result
              , slope = last result
              , pressStatistic = pressStatistic
              , running = false
            }
        )
      if isJust (head result) && isJust (last result)
        then do
          currentState' <- H.get
          _ <- H.fromAff (makePlot (makePlotDataFromState currentState'))
          pure next
        else pure next
      pure next

initialState :: State
initialState = {
      nextId: 0
    , points: []
    , yIntercept: Nothing
    , slope: Nothing
    , pressStatistic: Nothing
    , running: false
  }

newPoint :: Int -> Point
newPoint id = { id: id, x: Nothing, y: Nothing }

updatePointCoordinateFromString :: Point -> String -> String -> Point
updatePointCoordinateFromString point "x" s = point { x = stringToMaybeNumber s }
updatePointCoordinateFromString point "y" s = point { y = stringToMaybeNumber s }
updatePointCoordinateFromString point  _  _ = point

dataForLinearRegressionFromState ::
  State ->
  {
      maxIterations :: Int
    , maxCost       :: Number
    , learningRate  :: Number
    , coefficients  :: Array Number
    , regressands   :: Array Number
    , designMatrix  :: Array (Array Number)
  }
dataForLinearRegressionFromState state = {
      maxIterations: 10000
    , maxCost: 1e-11
    , learningRate: 0.05
    , coefficients: [1.0, 1.0]
    , regressands:  map (\ { y: y } ->  fromMaybe 0.0 y ) validPoints
    , designMatrix: map (\ { x: x } -> [fromMaybe 0.0 x]) validPoints
  }
  where
    validPoints = filter pointIsValid state.points

coords :: State -> Array (Array Number)
coords { points: [] }     = []
coords { points: points } = foldr (\ point acc ->
    if isNothing point.x || isNothing point.y
      then acc
      else [fromMaybe 0.0 point.x,  fromMaybe 0.0 point.y] : acc
  ) [] points

getXValuesFromState :: State -> Array (Number)
getXValuesFromState { points: points } = foldr (\ point acc ->
    if isNothing point.x || isNothing point.y
      then acc
      else fromMaybe 0.0 point.x : acc
  ) [] points

makePlotDataFromState :: State -> PlotData
makePlotDataFromState state@{
      points: points
    , yIntercept: (Just yIntercept)
    , slope: (Just slope)
  } = {
      scatter: pointsToScatterData points
    , line: if length xValues > 0
              then [
                    {
                        x: minX
                      , y: slope * minX + yIntercept
                    }
                  , {
                        x: maxX
                      , y: slope * maxX + yIntercept
                    }
                ]
              else []
  }
  where
    xValues = getXValuesFromState state
    minX    = arrayMinOrMax min 0.0 xValues
    maxX    = arrayMinOrMax max 0.0 xValues
makePlotDataFromState state@{
      points: points
    , yIntercept: Nothing
    , slope: Nothing
  } = {
      scatter: pointsToScatterData points
    , line: []
  }
makePlotDataFromState state@{
      points: points
    , yIntercept: (Just _)
    , slope: Nothing
  } = {
      scatter: pointsToScatterData points
    , line: []
  }
makePlotDataFromState state@{
      points: points
    , yIntercept: Nothing
    , slope: (Just _)
  } = {
      scatter: pointsToScatterData points
    , line: []
  }

pointsToScatterData :: Array Point -> Array { x :: Number, y :: Number }
pointsToScatterData []     = []
pointsToScatterData points = foldr (\ point acc ->
    if isNothing point.x || isNothing point.y
      then acc
      else { x: fromMaybe 0.0 point.x, y: fromMaybe 0.0 point.y } : acc
  ) [] points

hasValidPoints :: State -> Boolean
hasValidPoints { points: [] } = false
hasValidPoints state          = (length <<< coords) state > 0

pointIsValid :: Point -> Boolean
pointIsValid { id: _, x: (Just x), y: (Just y) } = true
pointIsValid _                                   = false

log' :: forall a b. Affable (console :: CONSOLE | b) a => String -> a Unit
log' string = H.fromAff $ when debug $ log string

debug :: Boolean
debug = false
