import qualified Data.Map.Strict as Map

example = [
        "abcdef",
        "bababc",
        "abbcde",
        "abcccd",
        "aabcdd",
        "abcdee",
        "ababab"
    ]

main :: IO ()
main = do 
    input <- readFile "input"
    let inputLines = lines input
    print $ solution inputLines
    print $ solution2 inputLines

-- Unlike Haskells' groupBy, characters need not be consecutive to go into the same group
groupBy' :: String -> Map.Map Char String
groupBy' str = let
    pairs = zip str $ map (:[]) str
    in Map.fromListWith (++) pairs

solution :: [String] -> Int
solution inputLines = let 
    groups = map groupBy' inputLines
    twice = filter (any (\x -> length x == 2)) groups
    thrice = filter (any (\x -> length x == 3)) groups
    in (length twice * length thrice)

example2 = [
        "abcde",
        "fghij",
        "klmno",
        "pqrst",
        "fguij",
        "axcye",
        "wvxyz"
    ]

levenshtein :: String -> String -> Int
levenshtein "" "" = 0
levenshtein (x:xs) (y:ys) 
    | x == y = levenshtein xs ys
    | otherwise = 1 + levenshtein xs ys
levenshtein _ _ = error "Cannot calculate levenshtein distance between words of different length"

dropDiffering :: String -> String -> String
dropDiffering x y = fst <$> filter (uncurry (==)) (zip x y)

solution2 :: [String] -> String
solution2 inputLines = head [dropDiffering x y | x <- inputLines, y <- inputLines, x /= y && levenshtein x y == 1]