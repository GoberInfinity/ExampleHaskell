import qualified Data.Map as M

solve :: (M.Map String Int, [(String, Int)]) -> (String, Int) -> (M.Map String Int, [(String, Int)])
solve (table, records) (name, score') =
  (M.insert name score table, (name, score) : records)
  where score = score' + M.findWithDefault 0 name table

parse :: [String] -> [(String, Int)]
parse [] = []
parse (name : score : rest) = (name, read score::Int) : parse rest

main :: IO ()
main = do
  input <- getContents

  let
    (table, records) = foldl solve (M.empty, []) (parse . tail . words $ input)
    highscore = maximum $ M.elems table
    cands = M.filter (== highscore) table
    isWinner (name, score) = score >= highscore && name `M.member` cands

  putStrLn . fst . last . filter isWinner $ records
