{-# LANGUAGE QuasiQuotes                      #-}

import Text.RE.Replace
import Text.RE.TDFA.String

data Claim = Claim {
    id :: String,
    x :: Int,
    y :: Int,
    width :: Int,
    height :: Int
} deriving (Show, Eq)

example = [
        Claim{Main.id="1", x=1, y=3, width=4, height=4},
        Claim{Main.id="2", x=3, y=1, width=4, height=4},
        Claim{Main.id="3", x=5, y=5, width=2, height=2}
    ]

main :: IO ()
main = do 
    input <- readFile "input"
    let inputLines = lines input
    let claims = map parseClaim inputLines
    print $ solution claims
    print $ solution2 claims

solution :: [Claim] -> Int
solution claims = length $ filter (\x -> length x > 1) [[(claim,x,y) | claim <- claims, contains claim x y] | x <- [1..1000], y <- [1..1000]]

solution2 :: [Claim] -> Claim
solution2 claims = head [ c1 | c1 <- claims, all (\c2 -> c2 == c1 || not (overlap c1 c2)) claims]

parseClaim :: String -> Claim
parseClaim str = let 
    match = str ?=~ [re|#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)|]
    id = match !$$  [cp|1|]
    x = match !$$  [cp|2|]
    y = match !$$  [cp|3|]
    w = match !$$  [cp|4|]
    h = match !$$  [cp|5|]
    in Claim{Main.id = id, x = read x, y = read y, width = read w, height = read h}

contains :: Claim -> Int -> Int -> Bool
contains Claim{x=x, y=y, width=w, height=h} x1 y1 =
    x <= x1 && x + w > x1 && y <= y1 && y + h > y1

overlap :: Claim -> Claim -> Bool
overlap Claim{x=x1, y=y1, width=w1, height=h1} Claim{x=x2, y=y2, width=w2, height=h2} = 
    not (left || right || above || below)
  where
    left = x1 + w1 <= x2
    right = x1 >= x2 + w2
    above = y1 + h1 <= y2
    below = y1 >= y2 + h2