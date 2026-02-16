module Model where

import Data.List (nub)

newtype Criterion = Criterion {unCriterion :: String}
  deriving (Eq, Ord)

instance Show Criterion where
  show (Criterion c) = c

newtype Alternative = Alternative {unAlternative :: String}
  deriving (Eq, Ord)

instance Show Alternative where
  show (Alternative a) = a

type Score = Double
newtype Weight = Weight {unWeight :: Double}
  deriving (Eq, Ord, Show)

-- (Alternative, Criterion, Score)
type ScoreTable = [(Alternative, Criterion, Score)]

-- (Criterion, Weight)
type Weights = [(Criterion, Weight)]

data ModelError
  = NegativeWeight Criterion Double
  | DuplicateWeight Criterion
  | WeightSumNotOne Double
  | DuplicateScore Alternative Criterion
  | MissingScore Alternative Criterion
  deriving (Eq, Show)

mkWeight :: Double -> Either String Weight
mkWeight w
  | w < 0 = Left "Weight must be non-negative"
  | otherwise = Right (Weight w)

weightValue :: Weight -> Double
weightValue (Weight w) = w

validateWeights :: Weights -> [ModelError]
validateWeights weights =
  negativeErrors ++ duplicateErrors ++ sumErrors
  where
    epsilon = 1e-9
    negativeErrors =
      [ NegativeWeight c w
      | (c, Weight w) <- weights
      , w < 0
      ]
    criteria = map fst weights
    duplicateErrors =
      [ DuplicateWeight c
      | c <- nub criteria
      , occurrences c criteria > 1
      ]
    totalWeight = sum [w | (_, Weight w) <- weights]
    sumErrors =
      [WeightSumNotOne totalWeight | abs (totalWeight - 1.0) > epsilon]

validateScoreTable :: ScoreTable -> Weights -> [ModelError]
validateScoreTable scores weights =
  duplicateErrors ++ missingErrors
  where
    pairs = [(a, c) | (a, c, _) <- scores]
    duplicateErrors =
      [ DuplicateScore a c
      | (a, c) <- nub pairs
      , occurrences (a, c) pairs > 1
      ]
    alternatives = nub [a | (a, _, _) <- scores]
    weightedCriteria = nub [c | (c, _) <- weights]
    missingErrors =
      [ MissingScore a c
      | a <- alternatives
      , c <- weightedCriteria
      , not (hasScore a c)
      ]
    hasScore a c = any (\(a', c', _) -> a == a' && c == c') scores

validateModel :: ScoreTable -> Weights -> [ModelError]
validateModel scores weights =
  validateWeights weights ++ validateScoreTable scores weights

occurrences :: Eq a => a -> [a] -> Int
occurrences x = length . filter (== x)
