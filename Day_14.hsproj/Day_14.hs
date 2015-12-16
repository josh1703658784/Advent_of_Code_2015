import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
import Data.List.Split

time=2503
--FORMAT: (name, distance, travel_time, rest_time)

main = do   
    c <- readFile "input.txt"
    if null c  i
        then return ()  
        else do  
          putStr $ show (maximum (calc_all_d (parse c)))


calc_all_d :: [(String, Int, Int, Int)] -> [Int]
calc_all_d xs = map (calc_d) xs

           

calc_d :: (String, Int, Int, Int) -> Int
calc_d (n,d,f,r) = d*(fly_time (big_time (zip r_l s_l)))
                  where r_l = running_list f r
                        s_l = sum_list r_l
                        big_time xs = fst (last (valid_time xs)) 
                        valid_time = takeWhile (\x -> snd x <= time) 
                        fly_time xs = sum (filter (==f) xs)



sum_list :: [[Int]] -> [Int]
sum_list xss = map (sum) xss

running_list :: Int -> Int -> [[Int]]
running_list f r = (foldl (combo) [[]] (take 1000 l)) 
                   where l = list_d f r
                         combo acc x = acc++[(last acc)++[x]]
                        

list_d :: Int -> Int -> [Int]
list_d x y = x:(list_d y x)

parse :: String -> [(String, Int, Int, Int)]
parse c = p_data (p_spaces (p_lines c))

p_lines :: String -> [String]
p_lines = splitOn "\n"

p_spaces :: [String] -> [[String]]
p_spaces xs = init (map (splitOn " ") xs)


p_data :: [[String]] -> [(String, Int, Int, Int)]
p_data xs = foldl (\acc x -> (n x, d x, f x, r x):acc) [] xs
              where n x = x!!0
                    d x = to_int (x!!3)
                    f x = to_int (x!!6)
                    r x = to_int (x!!13)
                    to_int x = read x :: Int
                    
