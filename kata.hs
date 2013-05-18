type Roll  = Int
type Score = Int

data Frame = Strike | Pair Roll Roll

rollsToFrames :: [Roll] -> Maybe [Frame]
rollsToFrames rolls = case rolls of
    []         -> Just []
    10:rolls'  -> fmap (Strike:)   (rollsToFrames rolls') 
    x:[]       -> Nothing
    x:y:rolls' -> fmap (Pair x y:) (rollsToFrames rolls')

score :: [Frame] -> Score
score = go 1 1
  where
    go m1 m2 rolls = case rolls of
        []              -> 0
        Strike:rolls'   -> 10 * m1 + go (m2 + 1) 2 rolls'
        Pair x y:rolls' ->
            x * m1 + y * m2
          + (if (x + y) == 10 then go 2 else go 1) 1 rolls'
