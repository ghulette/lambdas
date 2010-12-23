import LetRec.Expr
import LetRec.Parser

main :: IO ()
main = do
  inp <- getContents
  case parseExpr inp of
    Left err -> print err
    Right prg -> print (valueOfProgram prg)
