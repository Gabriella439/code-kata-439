type Roll  = Int
type Score = Int

score :: [Roll] -> Score
score = go 1 1
  where
    go m1 m2 rolls = case rolls of
        []         -> 0
        10:rolls'  -> 10 * m1 + go (m2 + 1) 2 rolls'
        x:[]       -> x  * m1
        x:y:rolls' ->
            x * m1 + y * m2
          + (if (x + y) == 10 then go 2 else go 1) 1 rolls'
