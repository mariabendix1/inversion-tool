-----------------
-- LATEX MAKER --
-----------------
-- Tool for generating Latex-files
module Tex where

import CCSAst
import CCSParser
import Inverter

import Data.List

makelatexname :: Task -> String
makelatexname (InvTask name i o) =
  -- map to 1-index
  name ++ "^{-1}_{\\{" ++ listToString i "," 
                       ++ "\\}\\{" 
                       ++ listToString o "," 
                       ++ "\\}}" 

makelatexconds :: [Cond] -> String
makelatexconds [] = ""
makelatexconds conds = "\\\\ \\;\\; \\Leftarrow " ++ 
                       makelatexconds' conds "first"

makelatexconds' :: [Cond] -> String -> String
makelatexconds' ((CArrow (ID name lterms) (ID "<>" rterms)):[]) num =
  let string = case num of
                 "first" -> ""
                 "second" -> "\\; \\; \\; \\; \\; \\; \\:"
      nameStr  = case parseFun name [] [] of
                   Left err -> err
                   Right mtask -> case mtask of
                                    (InvTask name [] []) -> name
                                    task -> makelatexname task
  in
  string ++ "\\mathsf{" ++ nameStr ++ "}(" ++ makelatexterms lterms 
  ++ ")" ++ "\\rightarrow \\langle " ++ makelatexterms rterms ++ 
  "\\rangle "

makelatexconds' ((CArrow (ID name lterms) (ID "<>" rterms)):conds) num =
  let string = case num of
                 "first" -> ""
                 "second" -> "\\; \\; \\; \\; \\; \\; \\:"
      nameStr  = case parseFun name [] [] of
                   Left err -> err
                   Right mtask -> case mtask of
                                    --Nothing -> name
                                    --Just (InvTask name [] []) -> name
                                    --Just task -> makelatexname task
                                    (InvTask name [] []) -> name
                                    task -> makelatexname task
  in
  string ++ "\\mathsf{" ++ nameStr ++ "}(" ++ makelatexterms lterms 
  ++ ")" ++ "\\rightarrow \\langle " ++ makelatexterms rterms ++ 
  "\\rangle \\; \\land \\\\"  ++ makelatexconds' conds "second"

makeflatlatexconds :: [Cond] -> String
makeflatlatexconds [] = ""
makeflatlatexconds conds = " \\Leftarrow " ++ makeflatlatexconds' conds "first"

makeflatlatexconds' :: [Cond] -> String -> String
makeflatlatexconds' ((CArrow (ID name lterms) (ID "<>" rterms)):[]) num = 
  let nameStr  = case parseFun name [] [] of
                   Left err -> err
                   Right mtask -> case mtask of
                                    --Nothing -> name
                                    (InvTask name [] []) -> name
                                    task -> makelatexname task
  in
  "\\mathsf{" ++ nameStr ++ "}(" ++ makelatexterms lterms ++ ")" ++
  "\\rightarrow \\langle " ++ makelatexterms rterms ++ "\\rangle "
makeflatlatexconds' ((CArrow (ID name lterms) (ID "<>" rterms)):conds) num =
  let nameStr  = case parseFun name [] [] of
                   Left err -> err
                   Right mtask -> case mtask of
                                    --Nothing -> name
                                    (InvTask name [] []) -> name
                                    task -> makelatexname task
  in
  "\\mathsf{" ++ nameStr ++ "}(" ++ makelatexterms lterms ++ ")" ++
  "\\rightarrow \\langle " ++ makelatexterms rterms ++ 
  "\\rangle \\; \\land "  ++ makeflatlatexconds' conds "second"

makelatexterms :: [Term] -> String
makelatexterms [] = ""
makelatexterms ((ID name []):[]) = name
makelatexterms ((VarID var []):[]) = var
makelatexterms ((ID name terms):[]) = name ++ "(" ++ 
                                      makelatexterms terms ++ ")"
makelatexterms ((ID name []):terms) = name ++ ", " ++
                                      makelatexterms terms
makelatexterms ((VarID var []):terms) = var ++ ", " ++
                                        makelatexterms terms
makelatexterms ((ID name terms):terms') = name ++ "(" ++
                                          makelatexterms terms ++ "), "
                                          ++ makelatexterms terms'

makelatex :: CCS -> String -> String
makelatex (Ccs {idlist = vars,
                funlist = sigs,
                rulelist = Rules []}) str = ""
makelatex (Ccs {idlist = vars,
                funlist = sigs,
                rulelist = Rules rules}) flat =
  "$$\\begin{array}{l@{}}\n" ++ makelatex' rules flat ++ 
  "\\\\ \\end{array}$$\n"

makelatex' :: [Rule] -> String -> String
makelatex' [] flat = ""
makelatex' ((Arrow (ID name lterms) 
                   (ID "<>" rterms)
                   conds):rules) flat =
  let condsStr = case flat of
                   "flat" -> makeflatlatexconds conds
                   _ -> makelatexconds conds
      nameStr  = case parseFun name [] [] of
                   Left err -> err
                   Right mtask -> case mtask of
                                    --Nothing -> name
                                    (InvTask name [] []) -> name
                                    task -> makelatexname task
  in
  "\\mathsf{" ++ nameStr ++"}(" ++ makelatexterms lterms ++ ") " ++
  "\\rightarrow \\langle " ++ makelatexterms rterms ++ " \\rangle"
  ++ condsStr ++ 
  "\\\\ \n" ++
  makelatex' rules flat

makelatexFromFile :: FilePath -> String -> String -> IO ()
makelatexFromFile path name flat = 
  --do let file = "tex/" ++ name ++ ".tex"
  do let file = name ++ ".tex"
     parsed <- parseCCSFile path
     case parsed of
          Left err -> putStr "Latex-maker failed"
          Right ccs -> let content = makelatex ccs flat
                       in writeFile file content
