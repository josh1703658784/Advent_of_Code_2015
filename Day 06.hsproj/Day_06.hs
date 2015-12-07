import Data.List.Split
--import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as V


main = do   
    contents <- readFile "input"
    if null contents  
        then return ()  
        else do  
            putStrLn $ show (run_show le_grid (parse_file contents))


le_grid = (replicate 1000 (replicate 1000 True))


grid_width = 1000

vector :: V.Vector Int
vector = V.fromList le_grid

--
--run_show' :: [[Bool]] -> [(Int, (Int, Int), (Int, Int))] -> [[Bool]]
--run_show' grid inst =  (foldl (\acc x -> update_lights acc x) grid inst)
--                            where update_lights acc i = update_lights acc i
--
run_show :: [[Bool]] -> [(Int, (Int, Int), (Int, Int))] -> Int
run_show grid inst = length (map (filter (\b -> True)) (foldl (\acc x -> update_lights acc x) grid inst))
                            where update_lights acc i = update_lights acc i




--update_lights :: [[Bool]] -> (Int, (Int, Int), (Int, Int)) -> [[Bool]]
update_lights grid (0, (x1, y1), (x2, y2)) = [f (grid!!x!!y) | x <- [x1..x2], y <- [y1..y2]]
                                            where f = (&& False)
update_lights grid (1, (x1, y1), (x2, y2)) = [f (grid!!x!!y) | x <- [x1..x2], y <- [y1..y2]]
                                            where f = (|| True)
update_lights grid (2, (x1, y1), (x2, y2)) = [f (grid!!x!!y) | x <- [x1..x2], y <- [y1..y2]]
                                            where f = (not)


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
parse_position xs = (action_num, (first_position action_num xs), (second_position action_num xs))
                    where action_num = action xs

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

action :: [String] -> Int
action xs = toggle + on + off
          where toggle  = if (xs!!0 == "toggle") then toggle_num else 0
                on      = if (xs!!1 == "on") then on_num else 0
                off     = if (xs!!1 == "off") then off_num else 0

