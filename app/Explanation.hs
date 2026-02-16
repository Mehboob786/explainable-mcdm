module Explanation where

import Model

-- Contribution of a single criterion when comparing two alternatives
criterionContribution ::
  Alternative -> Alternative -> Criterion ->
  ScoreTable -> Weights -> Double
criterionContribution a b c scores weights =
  let sa = sum [ s * weightValue w
               | (alt, crit, s) <- scores
               , alt == a
               , crit == c
               , (c', w) <- weights
               , c == c'
               ]
      sb = sum [ s * weightValue w
               | (alt, crit, s) <- scores
               , alt == b
               , crit == c
               , (c', w) <- weights
               , c == c'
               ]
  in sa - sb

-- Explain how a criterion's contribution changes over time
explainCriterionChange ::
  Alternative -> Alternative -> Criterion ->
  ScoreTable -> Weights -> Weights -> String
explainCriterionChange a b c scores w0 w1 =
  let d0 = criterionContribution a b c scores w0
      d1 = criterionContribution a b c scores w1
  in "Criterion \"" ++ show c ++ "\" changed contribution from "
     ++ show d0 ++ " to " ++ show d1
