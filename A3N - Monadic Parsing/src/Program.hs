module Program(T, parse, fromString, toString, exec) where

import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

instance Eq T where
  p1 == p2 = toString p1 == toString p2

instance Show T where
  show = toString

instance Parse T where
  --parse = error "Program.parse not implemented"
  parse = (spaces -# iter Statement.parse #- spaces) >-> Program
  --toString = error "Program.toString not implemented"
  toString (Program stmts) = concatMap Statement.toString stmts

exec :: T -> [Integer] -> [Integer]
--exec = error "Program.exec not implemented"
exec (Program stmts) =
  Statement.execute stmts Dictionary.empty 