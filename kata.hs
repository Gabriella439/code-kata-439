import Data.List ( inits, intersperse )
import Data.Traversable ( traverse )


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


showFrameDisplay :: [Frame] -> [String]
showFrameDisplay = showFrameRow

showFrameRow :: [Frame] -> [String]
showFrameRow frames = map formatCells
    [ map showFrame $ frames
    , map (show . score) . tail . inits $ frames
    ]
  where
    pad :: Int -> String -> String
    pad n s = replicate (n - length s) ' ' ++ s

    extend :: Int -> [[a]] -> [[a]]
    extend n xs = xs ++ replicate (n - length xs) []

    formatCells :: [String] -> String
    formatCells = concat . intersperse "|" . map (pad 3) . extend 10


type Game = [[Roll]]

showGame :: Game -> Maybe [[String]]
showGame = traverse (fmap showFrameRow . rollsToFrames)


displayFrames :: [Frame] -> IO ()
displayFrames = mapM_ putStrLn . showFrameDisplay

displayRolls :: [Roll] -> IO ()
displayRolls = maybe showError displayFrames . rollsToFrames
  where

    showError = putStrLn "invalid rolls"
displayGame :: Game -> IO ()
displayGame = maybe showErr ((mapM_.mapM_) putStrLn) . showGame
  where
    showErr = putStrLn "invalid game"
