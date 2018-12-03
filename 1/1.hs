import Data.List

main :: IO ()
main = do 
    input <- readFile "input"
    let numbers = map (\x -> read (filter (/= '+') x) :: Int) $ lines input
    print (sum numbers)
    print $ firstDuplicate $ cumulSum numbers

cumulSum :: [Int] -> [Int]
cumulSum numbers = let
    stream = concat $ repeat numbers
    in scanl1 (+) stream

firstDuplicate :: [Int] -> Maybe Int
firstDuplicate numbers = fst <$> find (uncurry elem) numbersWithPrefixes
  where
    -- A prefix is defined as the list of all elements before the given index
    numberPrefixes = scanl (flip (:)) ([] :: [Int]) numbers
    numbersWithPrefixes = zip numbers numberPrefixes
