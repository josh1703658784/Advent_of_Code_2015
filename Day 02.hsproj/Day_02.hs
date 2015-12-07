import Data.List.Split
import Data.List


main = do   
    contents <- readFile "input.txt"
    if null contents  
        then return ()  
        else do  
            putStrLn $ show (totalArea(parseFile contents))
            putStrLn $ show (totalRibbon(parseFile contents))


totalArea :: [(Int, Int, Int)] -> Int
totalArea xs = foldl (\acc x -> (acc + (partialArea x))) 0 xs

partialArea :: (Int, Int, Int) -> Int
partialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
                      where smallSides  = take 2 (sort [l, w, h])
                            slack       = foldl1 (*) smallSides
                            




totalRibbon :: [(Int, Int, Int)] -> Int
totalRibbon xs = foldl (\acc x -> (acc + (partialRibbon x))) 0 xs


partialRibbon :: (Int, Int, Int) -> Int
partialRibbon (l, w, h) = 2*(foldl1 (+) smallSides) + slack
                        where sides = [l,w,h]
                              smallSides = take 2 (sort sides)
                              slack      = foldl1 (*) sides





parseFile :: String -> [(Int, Int, Int)]
parseFile xs = map (splitDimensions) (breakLines xs)


breakLines :: String -> [String]
breakLines = splitOn "\n"


splitDimensions :: String -> (Int, Int, Int)
splitDimensions xs = (item 0, item 1, item 2)
                   where item n = read ((splitOn "x" xs)!!n)
                   
