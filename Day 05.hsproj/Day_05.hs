-- *************************************
--NOT FUNCTIONING **********************
-- *************************************



import Data.List.Split
import Data.List


main = do   
    contents <- readFile "input.txt"
    if null contents  
        then return ()  
        else do  
            putStrLn $ show (parse_file contents)
            



parse_file :: String -> [String]
parse_file = splitOn "\n"
            

--
--filter_strings :: [String] -> [String]
--filter_strings xs = filter (vowels) xs
--                    where vowels xs = length (elem [aeiou] xs)


-- *************************************
--NOT FUNCTIONING **********************
-- *************************************