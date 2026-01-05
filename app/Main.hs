module Main where

import Model
import Scoring
import Explanation

-- Example score table
scores :: ScoreTable
scores =
  [ ("A", "Cost",    7)
  , ("A", "Quality", 8)
  , ("A", "Speed",   6)
  , ("B", "Cost",    5)
  , ("B", "Quality", 9)
  , ("B", "Speed",   7)
  ]

-- Initial weights
weightsT0 :: Weights
weightsT0 =
  [ ("Cost", 0.3)
  , ("Quality", 0.4)
  , ("Speed", 0.3)
  ]

-- Updated weights (dynamic change)
weightsT1 :: Weights
weightsT1 =
  [ ("Cost", 0.4)
  , ("Quality", 0.35)
  , ("Speed", 0.25)
  ]

main :: IO ()
main = do
  putStrLn "=== Explainable Multi-Criteria Decision-Making ==="

  let a = "A"
  let b = "B"

  putStrLn "\n--- Scores at time T0 ---"
  print (weightedSum a scores weightsT0)
  print (weightedSum b scores weightsT0)

  putStrLn "\n--- Scores at time T1 ---"
  print (weightedSum a scores weightsT1)
  print (weightedSum b scores weightsT1)

  putStrLn "\n--- Explanation of Changes ---"
  putStrLn (explainCriterionChange a b "Cost" scores weightsT0 weightsT1)
  putStrLn (explainCriterionChange a b "Quality" scores weightsT0 weightsT1)
  putStrLn (explainCriterionChange a b "Speed" scores weightsT0 weightsT1)
