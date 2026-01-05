module Model where

type Criterion = String
type Alternative = String
type Score = Double
type Weight = Double

-- (Alternative, Criterion, Score)
type ScoreTable = [(Alternative, Criterion, Score)]

-- (Criterion, Weight)
type Weights = [(Criterion, Weight)]
