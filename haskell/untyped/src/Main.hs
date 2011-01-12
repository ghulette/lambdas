import Parser
import Expr
import Interp
import System.Exit

parseInput :: String -> IO Expr
parseInput x = 
  case parseExpr x of
    Left err -> do
      print err
      exitFailure
    Right expr -> 
      return expr

main :: IO ()
main = do
  input <- getContents
  expr <- parseInput input
  run expr
