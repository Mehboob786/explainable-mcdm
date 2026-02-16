module Main where

import Control.Monad (forM_)
import Model
import Scoring
import Explanation

-- Example score table
scores :: ScoreTable
scores =
  [ (a, cost,    7)
  , (a, quality, 8)
  , (a, speed,   6)
  , (b, cost,    5)
  , (b, quality, 9)
  , (b, speed,   7)
  ]

-- Initial weights
weightsT0 :: Weights
weightsT0 =
  [ (cost,    Weight 0.3)
  , (quality, Weight 0.4)
  , (speed,   Weight 0.3)
  ]

-- Updated weights (dynamic change)
weightsT1 :: Weights
weightsT1 =
  [ (cost,    Weight 0.4)
  , (quality, Weight 0.35)
  , (speed,   Weight 0.25)
  ]

main :: IO ()
main = do
  putStrLn "=== Explainable Multi-Criteria Decision-Making ==="

  let modelErrorsT0 = validateModel scores weightsT0
  let modelErrorsT1 = validateModel scores weightsT1

  if null (modelErrorsT0 ++ modelErrorsT1)
    then do
      putStrLn "\n--- Scores at time T0 ---"
      print (weightedSum a scores weightsT0)
      print (weightedSum b scores weightsT0)

      putStrLn "\n--- Scores at time T1 ---"
      print (weightedSum a scores weightsT1)
      print (weightedSum b scores weightsT1)

      putStrLn "\n--- Explanation of Changes ---"
      putStrLn (explainCriterionChange a b cost scores weightsT0 weightsT1)
      putStrLn (explainCriterionChange a b quality scores weightsT0 weightsT1)
      putStrLn (explainCriterionChange a b speed scores weightsT0 weightsT1)
    else do
      putStrLn "\nModel validation failed:"
      forM_ (modelErrorsT0 ++ modelErrorsT1) print

cost, quality, speed :: Criterion
cost = Criterion "Cost"
quality = Criterion "Quality"
speed = Criterion "Speed"

a, b :: Alternative
a = Alternative "A"
b = Alternative "B"
