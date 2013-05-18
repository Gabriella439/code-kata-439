type Roll  = Int
type Score = Int

data Frame = Strike | Pair Roll Roll

rollsToFrames :: [Roll] -> Maybe [Frame]
rollsToFrames rolls = case rolls of
    []         -> Just []
    10:rolls'  -> fmap (Strike:)   (rollsToFrames rolls') 
    x:[]       -> Nothing
    x:y:rolls' -> fmap (Pair x y:) (rollsToFrames rolls')

framesToRolls :: [Frame] -> [Roll]
framesToRolls frames = case frames of
    []            -> []
    Pair x y : xs -> x : y : framesToRolls xs
    Strike : xs   -> 10 : framesToRolls xs

score :: [Frame] -> Score
score = go 1 1
  where
    go m1 m2 rolls = case rolls of
        []              -> 0
        Strike:rolls'   -> 10 * m1 + go (m2 + 1) 2 rolls'
        Pair x y:rolls' ->
            x * m1 + y * m2
          + (if (x + y) == 10 then go 2 else go 1) 1 rolls'

showFrame :: Frame -> String
showFrame frame = case frame of
    Strike   -> " X"
    Pair x y
      | x + y == 10 -> showRoll x ++ "/"
      | otherwise   -> showRoll x ++ showRoll y
  where
    showRoll 0 = "-"
    showRoll x = show x
