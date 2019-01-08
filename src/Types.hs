module Types where

data LispVal
  = Atom String
  | Number Integer
  | String String
  | Bool Bool
  | ProperList [LispVal]
  | ImproperList [LispVal] LispVal
  deriving (Eq)

instance Show LispVal where
  show (Atom a) = a
  show (Number i) = show i
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (ProperList l) = "(" ++ (showLispList l) ++ ")"
  show (ImproperList l x) = "(" ++ (showLispList l) ++ show x ++ ")"

showLispList :: [LispVal] -> String
showLispList = unwords . map show

