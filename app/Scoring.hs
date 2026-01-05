module Scoring where

import Model

-- Weighted Sum Model
weightedSum :: Alternative -> ScoreTable -> Weights -> Double
weightedSum alt scores weights =
  sum
    [ s * w
    | (a, c, s) <- scores
    , a == alt
    , (c', w) <- weights
    , c == c'
    ]
