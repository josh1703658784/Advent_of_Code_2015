import Data.List.Split
--import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as V
--import qualified Data.Array.Repa as R
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -XTypeOperators #-}


main = do   
    contents <- readFile "input.txt"
    if null contents  
        then return ()  
        else do  
            putStrLn $ show grid_size
            putStrLn $ show (run_show lights_grid (parse_file contents))


grid_width = 1000
grid_size = grid_width*grid_width
lights_grid = replicate 1000000 False



run_show :: [Bool] -> [(Int, (Int, Int), (Int, Int))] -> Int
run_show g [] =  length (filter (==True) g)
run_show g (i:is) = run_show (run_inst g i) is


run_inst :: [Bool] -> (Int, (Int, Int), (Int, Int)) -> [Bool]
run_inst g (i, (x1,y1),(x2,y2)) = [(if (pair_n n) `p_eq` (x1,y1) == GT && (pair_n n) `p_eq` (x2,y2) == LT  then update g n else same g n) | n <- [0..grid_size]] --[update g x y | x <- [x1..x2], y <- [y1..y2]]
                                where f = action_partial i
                                      update g n = f (g!!n)
                                      same g n = (g!!n)
                                      pair_n n = to_2d n

                  


inside_change :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
inside_change a (b, c) = (a `p_eq` b == LT && a `p_eq` c == GT)


--p_eq :: (Ordering o) => (Int, Int) -> (Int, Int) -> o
(a,b) `p_eq` (c,d) | a < c && b < d = LT
                   | a > c && b > d = GT
                   | a == c && b == d = EQ
                   | otherwise = EQ

to_2d :: Int -> (Int, Int)
to_2d n = (n `mod` grid_width, n `div` grid_width)


to_1d :: Int -> Int -> Int
to_1d x y = (x + (y*1000))
--
--no_change :: Bool -> Bool
--no_change =

--PARSING
toggle_num = 2
on_num = 1
off_num = 0

parse_file :: [Char] -> [(Int, (Int, Int), (Int, Int))]
parse_file xs = tail (foldl (\acc x -> (parse_position(break_spaces x)):acc) [] (tail(break_lines xs)))

break_lines :: [Char] -> [[Char]]
break_lines = splitOn "\n"

break_spaces :: [Char] -> [[Char]]
break_spaces = (splitOn " ")

parse_position :: [[Char]] -> (Int, (Int, Int), (Int, Int))
parse_position xs = (n, (first_position n xs), (second_position n xs))
                    where n = action_num xs

first_position :: Int -> [String] -> (Int, Int)
first_position action xs = (read (split_pos !! 0), read (split_pos !! 1))
                          where index = if action == toggle_num then 1 else 2
                                pos = xs!!index
                                split_pos =  (splitOn "," pos) 
                                
second_position :: Int -> [String] -> (Int, Int)
second_position action xs = (read (split_pos !! 0), read (split_pos !! 1))
                          where index = if action == toggle_num then 3 else 4
                                pos = xs!!index
                                split_pos =  (splitOn "," pos)

action_num :: [String] -> Int
action_num xs = toggle + on + off
              where toggle  = if (xs!!0 == "toggle") then toggle_num else 0
                    on      = if (xs!!1 == "on") then on_num else 0
                    off     = if (xs!!1 == "off") then off_num else 0


action_partial :: Int -> (Bool -> Bool)
action_partial 0 = (&&False)
action_partial 1 = (||True)
action_partial 2 = (not)
