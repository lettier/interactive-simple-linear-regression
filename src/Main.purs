{-
  (C) 2017 David Lettier
  lettier.com
-}

module Main where

import Prelude

import Data.Maybe (fromMaybe)

import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.Util (awaitBody, runHalogenAff, selectElement)

import Plot (emptyPlotData, makePlot)

import UI (Effects, ui, initialState)

main :: Eff (Effects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  uiContainer <- selectElement "#uiContainer"
  H.runUI ui initialState (fromMaybe body uiContainer)
  makePlot emptyPlotData
