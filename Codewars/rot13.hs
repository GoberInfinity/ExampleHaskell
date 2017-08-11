import Data.Char
rot13 x = let numbers = map (13+) $ map ord x
              converted = [if number > 122 then 96 + (number - 122) else number | number <- numbers]
              in map chr converted

