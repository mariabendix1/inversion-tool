---------------------
-- ABSTRACT SYNTAX --
---------------------
-- Inspired by http://project-coco.uibk.ac.at/problems/ctrs.php
-- Assuming correct input

module CCSAst where

-- error message
type ErrMsg = String

-- Id is a sequence of characters
type Id = String

-- differentiate between var terms and cons/fun terms
data Term = ID Id [Term] -- term can be empty
          | VarID Id [Term]
          deriving (Eq, Read, Show) 

-- rule ::= l -> r
--        | l -> r <= c
data Rule = Arrow Term Term [Cond] -- , [Cond] can be empty
          deriving (Eq, Read, Show) 
 
-- in http://project-coco.uibk.ac.at/problems/ctrs.php, we have:
-- cond ::= term == term, ...
-- in 'inversion framework', we have:
-- cond ::= l1 -> r1 ^ l2 -> r2 ^ ....
-- Which both can be represented as:
data Cond = CArrow Term Term
          deriving (Eq, Read, Show) 

-- VAR idlist
newtype VAR = Var [Id]
  deriving (Eq, Read, Show)

-- SIG idlist
newtype SIG = Sig [(Id, Int, Int)]
  deriving (Eq, Read, Show)

-- RULES rulelist
newtype RULES = Rules [Rule]
  deriving (Eq, Read, Show)

-- ccs ::= (VAR idlist) (RULES rulelist)
data CCS = Ccs {idlist :: VAR, funlist :: SIG, rulelist :: RULES}
  deriving (Eq, Read, Show)

-- an inversion task consists of:
-- * the function id
-- * index set (input)
-- * index set (output)
--
-- Maybe use the datatype 'Set e' instead?
data Task = InvTask Id [Int] [Int]
  --deriving (Eq, Show)
  deriving (Eq, Show)

