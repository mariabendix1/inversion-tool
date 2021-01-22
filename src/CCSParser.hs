------------
-- PARSER --
------------
-- Parse from CCS format, either Kirkeby and Glueck or
-- http://project-coco.uibk.ac.at/problems/ctrs.php
--
module CCSParser where

import CCSAst

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Either
import Control.Monad (void)

type Parser a = Parsec String () a

-- for later :-)
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t\n"

--"whitespace" everywhere is ugly.
-- When there is time, try: 
lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

eol :: Parser ()
eol = void (char '\n') <|> eof

listToString :: Show a => [a] -> String -> String
listToString [] delim = ""
listToString [x] delim = show x
listToString (x:xs) delim = (show x) ++ delim ++ (listToString xs delim)

--------------------------
-- Function name parser -- 
--------------------------
-- The following "extracts" a task from a function name. E.g. from
-- add{0}{1}, we have the task 'InvTask "add" [0] [1]'
--
-- listParser head
parseHead :: Parser Int
parseHead = do n <- many1 digit
               return (read n)

parseTail :: Parser [Int]
parseTail = do char '}'
               return []
            <|> do void $ char ','
                   head <- parseHead
                   tail <- parseTail
                   return (head:tail)
            <|> do head <- parseHead
                   tail <- parseTail
                   return (head:tail)

--funToTask :: [Int] -> [Int] -> Parser (Maybe Task)
funToTask :: [Int] -> [Int] -> Parser Task
funToTask i o = do name <- ident
                   funToTask' name i o

--funToTask' :: Id -> [Int] -> [Int] -> Parser (Maybe Task)
funToTask' :: Id -> [Int] -> [Int] -> Parser Task
funToTask' name i o = do eof
                         return (InvTask name i o)
                         --return (Just (InvTask name i o))
                      <|> 
                      do void $ char '{'
                         ilist <- parseTail
                         void $ char '{'
                         olist <- parseTail
                         funToTask'' name ilist olist

funToTask'' :: Id -> [Int] -> [Int] -> Parser Task
funToTask'' name i o = do eof
                          return (InvTask name i o)
                       <|>
                       do void $ many (char '_')
                          void $ char '{'
                          ilist <- parseTail
                          void $ char '{'
                          olist <- parseTail
                          return (InvTask (name ++ "{" ++ (listToString i ",")++"}{"++(listToString o ",")++"}") ilist olist)                         

--funToTask :: [Int] -> [Int] -> Parser (Maybe Task)
funToTaskDouble :: [Int] -> [Int] -> Parser Task
funToTaskDouble i o = do name <- ident
                         funToTaskDouble' name i o

--funToTask' :: Id -> [Int] -> [Int] -> Parser (Maybe Task)
funToTaskDouble' :: Id -> [Int] -> [Int] -> Parser Task
funToTaskDouble' name i o = do eof
                               return (InvTask name i o)
                               --return (Just (InvTask name i o))
                            <|> 
                            do void $ char '{'
                               ilist <- parseTail
                               void $ char '{'
                               olist <- parseTail
                               return (InvTask (name++"{"++(listToString ilist ",")++"}{"++(listToString olist ",")++"}") i o)
                         --return (Just (InvTask name ilist olist))
-----------------------------------
--         Io-set prettify       --
-----------------------------------
-- For printing the correct io-sets
--
-- I did not follow the 1-index convention when I started writing
-- this program. This, I regret now.
-- Fix: When printing, just add 1 to everything
funToPretty :: Parser Id
funToPretty = do name <- ident
                 funToPretty' name

funToPretty' :: Id -> Parser Id
funToPretty' name = do eof
                       return name
                    <|> 
                    do void $ char '{'
                       ilist <- parseTail
                       void $ char '{'
                       olist <- parseTail
                       funToPretty'' name ilist olist

funToPretty'' :: Id -> [Int] -> [Int] -> Parser Id
funToPretty'' name ilist olist = 
                       do eof
                          return (name ++ "{" ++
                               (listToString 
                                  (map (\x -> x + 1) ilist) ",") ++
                               "}{" ++
                               (listToString
                                  (map (\x -> x + 1) olist) ",") ++
                               "}")
                    <|> 
                     do void $ many (char '_')
                        void $ char '{'
                        ilist2 <- parseTail
                        void $ char '{'
                        olist2 <- parseTail
                        funToPretty''' name ilist2 olist2

funToPretty''' :: Id -> [Int] -> [Int] -> Parser Id
funToPretty''' name ilist2 olist2 =
                     do eof
                        return (name ++ "{" ++
                               (listToString 
                                  (map (\x -> x + 1) ilist2) ",") ++
                               "}{" ++
                               (listToString
                                  (map (\x -> x + 1) olist2) ",") ++
                               "}")
                     <|>
                     do void $ many1 (char '_')
                        void $ char '{'
                        ilist3 <- parseTail
                        void $ char '{'
                        olist3 <- parseTail
                        return (name ++ "{" ++
                               (listToString ilist2 ",") ++
                               "}{" ++
                               (listToString olist2 ",") ++
                               "}_"++
                               (listToString 
                                  (map (\x -> x + 1) ilist3) ",") ++
                               "}{" ++
                               (listToString
                                  (map (\x -> x + 1) olist3) ",") ++
                               "}")
                        

-- parse io-sets
iosets :: Parser Id
iosets = do io <- many1 $ choice [oneOf "{},", digit]
            return io
         <|> return []

funFromTask :: Parser Id
funFromTask = do name <- ident
                 brackets <- iosets
                 return (name++brackets)

--------------------------
-- Run fun name parsers --
--------------------------
parseFun :: String -> [Int] -> [Int] -> Either ErrMsg Task
--parseFun :: String -> [Int] -> [Int] -> Either ErrMsg (Maybe Task)
parseFun fun i o = case runParser (funToTask i o) () ("<error>" ++ fun) fun of
                     Right maybetask -> Right maybetask
                     Left err -> error (show err)

parseFunDouble :: String -> [Int] -> [Int] -> Either ErrMsg Task
--parseFun :: String -> [Int] -> [Int] -> Either ErrMsg (Maybe Task)
parseFunDouble fun i o = case runParser (funToTaskDouble i o) () "<error>" fun of
                     Right maybetask -> Right maybetask
                     Left err -> error (show err)

parseFunPretty :: Id -> Either ErrMsg Id
parseFunPretty name = case runParser funToPretty () ("<error>" ++ name) name of
                        Right id -> Right id
                        Left err -> error (show err)

----------------
-- CCS PARSER --
----------------
-- parsing can result in an error or in a CCS structure
--
-- define some keywords
-- more symbols could be allowed
languageDef =
  emptyDef { T.identStart = alphaNum <|> oneOf "[:]+-^=",
             T.identLetter = alphaNum <|> oneOf "_-[]:+='",
             -- alt fra termer
             T.reservedNames = ["VAR", "SIG", "RULES", "CONDITIONTYPE", 
                                "COMMENT", "==", "->", "<=", 
                                "|", ",", "^"]
           }

lexer = T.makeTokenParser languageDef

reserved = T.reserved lexer
ident = T.identifier lexer

------------
-- idlist --
------------
-- the list of variables is separated by a space character, it seems.
-- we accept any whitespace
varlist :: Parser [Id]
varlist = do var <- ident
             whitespace
             vars <- varlist
             return (var:vars)
          <|> do return []

siglist :: Parser [(Id, Int, Int)]
siglist = do reserved "SIG"
             whitespace
             siglist'
          <|> do return []

siglist' :: Parser [(Id, Int, Int)]
siglist' = do void $ char '('
              sig <- ident
              whitespace
              i <- many1 digit
              whitespace
              o <- many1 digit
              void $ char ')'
              whitespace
              sigs <- siglist'
              return ((sig, (read i), (read o)):sigs)
           <|> do void $ char ')'
                  whitespace
                  void $ char '('
                  return []

-------------------
-- single output --
-------------------
-- hacky solution for CoCo format
-- used in the case where the output of a rule or a condition only
-- consists of one element
single :: [Id] -> Id -> Parser [Term]
single idlist id =
  do void $ char '('
     terms <- termlist idlist
     void $ char ')'
     return [ID id terms]
  <|> do case (id `elem` idlist) of
           True -> return [VarID id []]
           False -> return [ID id []]

-- the rhs of a term can be in ang brackets <> or in brackets ()
-- or it might "stand alone" in which case we need to run 'single'
termlistR :: [Id] -> Parser [Term]
termlistR idlist = 
  do void $ char '('
     ret <- termlist idlist
     void $ char ')'
     return ret
  <|>
  do void $ char '<'
     whitespace
     ret <- termlist idlist
     void $ char '>'
     return ret
  <|>
  do id <- ident
     single idlist id
  <|> do return []
                       
-- parses id in termlist
termlist :: [Id] -> Parser [Term]
termlist idlist = 
  do id <- ident
     termlist' idlist id
  <|> do return []

termlist' :: [Id] -> Id -> Parser [Term]
termlist' idlist id =
  -- case function/constructor e.g. f(x)
  do void $ char '('
     id' <- ident
     terms <- termlist' idlist id'
     void $ char ')'
     termlistConstr idlist (ID id terms)
  <|>
  -- next element of term
  do void $ char ','
     whitespace 
     id' <- ident
     ids <- termlist' idlist id'
     -- is id a variable?
     case (id `elem` idlist) of 
       True -> return ((VarID id []):ids)
       False -> return ((ID id []):ids)
      -- no more elements - is id a variable?
  <|> do case (id `elem` idlist) of
           True -> return [VarID id []]
           False -> return [ID id []]

-- case constructors/functions, 
-- should be able to merge with termlist'
termlistConstr :: [Id] -> Term -> Parser [Term]
termlistConstr vars term = 
  do void $ char ','
     whitespace 
     elems <- termlist vars
     return (term:elems)
  <|> do return [term]

-- parsing conditions, similar to rule parser
--
-- we need both condlist and condlist' for deterministic choice.
-- because we want '<=' or '|' before the condition list, and 
-- ',' and '^' as condlist delimiters
condlist' :: [Id] -> Parser [Cond]
condlist' vars = 
  do choice [reserved ",", reserved "^"]
     whitespace
     funname <- lexeme funFromTask

     void $ char '('
     termlistl <- termlist vars
     void $ char ')'

     whitespace
     choice [reserved "->", reserved "=="]
     whitespace

     termlistr <- termlistR vars
     whitespace

     conds <- condlist' vars

     return ((CArrow (ID funname termlistl) (ID "<>" termlistr)):conds)
  <|> do return []

condlist :: [Id] -> Parser [Cond]
condlist vars = 
  do choice [reserved "<=", reserved "|"]
     whitespace
     funname <- lexeme funFromTask

     void $ char '('
     termlistl <- termlist vars
     void $ char ')'

     whitespace
     choice [reserved "->", reserved "=="]
     whitespace

     termlistr <- termlistR vars
     whitespace

     conds <- condlist' vars

     return ((CArrow (ID funname termlistl) (ID "<>" termlistr)):conds)
  <|> do return []

-- rulelist, p for parser, bad naming
rulelistP :: [Id] -> Parser [Rule]
rulelistP vars = 
  do -- lhs function
     funname <- lexeme funFromTask
     void $ char '('
     termlistl <- termlist vars
     void $ char ')'

     whitespace
     choice [reserved "->", reserved "=="]
     whitespace

     termlistr <- termlistR vars
     whitespace

     conds <- condlist vars

     whitespace

     rest <- rulelistP vars     
     return ((Arrow (ID funname termlistl) (ID "<>" termlistr) conds):rest)
   <|> do return []


-- ignore (CONDITIONTYPE ....)
condtype :: Parser ()
condtype = do void $ string "CONDITIONTYPE "
              skipMany1 $ choice [alphaNum, oneOf "-_"]
              void $ char ')'
              whitespace
              void $ char '('

-----------------
-- MAIN PARSER --
-----------------
-- ccs ::= ( CONDTYPE ) (VAR ) ( RULES )
ccsParser :: Parser CCS
               -- condtype is optional for now
ccsParser = do void $ char '('
               optional condtype
              
               -- parse VAR
               reserved "VAR"
               vars <- varlist
               void $ char ')'

               whitespace    

               -- parse SIG
               void $ char '('
               sigs <- siglist

               whitespace    

               -- parse RULES
               reserved "RULES"
               rules <- rulelistP vars
               void $ char ')'

               -- parse COMMENT
               optional $ char '('
               optional $ reserved "COMMENT"
             
               -- putting everything together :-)
               return $ Ccs {idlist = Var vars, 
                             funlist = Sig sigs,
                             rulelist = Rules rules}

-- run parser
parseCCS :: String -> Either ErrMsg CCS
parseCCS str = case runParser (lexeme ccsParser) () "<error>" str of
                 Right ccs -> Right ccs
                 Left err -> Left (error (show err))

-- run parser on file
parseCCSFile :: FilePath -> IO (Either ErrMsg CCS)
parseCCSFile path = (fmap parseCCS $ readFile path)

