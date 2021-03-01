module CCSPretty where

import CCSAst
import CCSParser
import Inverter
import CCSProps

import Data.List

--------------------
-- PRETTY PRINTER --
--------------------
-- Here, things are printed prettily.

----------------------------------------------------------------
-- Analyze type of inversion w. respect to original signature --
----------------------------------------------------------------
-- Input: new i, new o, orig i, orig o
invType :: Int -> Int -> Int -> Int -> String
invType i o i' o'
  | i == o' && o == i' = "Full" -- ++ (show i) ++ (show o) ++ (show i')
  | i >= o' && o <= i' = "Partial" -- ++ (show i) ++ (show o) ++ (show i')

  | otherwise = "Semi"

---------------------
-- PRETTY PRINTING --
---------------------
-- Make I/O-sets readable
makeprettyname :: Id -> Id
makeprettyname id = case parseFunPretty id of
                      --Right id' -> id'
                      Right id' -> id'
                      Left err -> err
-- Print condition list
makeprettyconds :: [Cond] -> String
makeprettyconds [] = ""
makeprettyconds ((CArrow lterm rterm):conds) =
  if conds == [] then 
    (makeprettyterm lterm) ++ " -> " ++ (makeprettyterm rterm)
  else
    (makeprettyterm lterm) ++ " -> " ++ (makeprettyterm rterm) ++ " ^ "
    ++ (makeprettyconds conds)

-- Print terms
makeprettyterms :: [Term] -> String
makeprettyterms [] = ""
makeprettyterms ((ID name []):[]) = (makeprettyname name)
makeprettyterms ((ID name []):terms) = 
  (makeprettyname name) ++ ", " ++ (makeprettyterms terms)
makeprettyterms ((VarID name []):[]) = name
makeprettyterms ((VarID name []):terms) = 
  name ++ ", " ++ (makeprettyterms terms)
makeprettyterms ((ID name rest):[]) = 
  (makeprettyname name) ++ "(" ++ (makeprettyterms rest) ++ ")" 
makeprettyterms ((ID name rest):terms) = 
  (makeprettyname name) ++ "(" ++ (makeprettyterms rest) ++ "), " 
  ++ (makeprettyterms terms)

makeprettyterm :: Term -> String
makeprettyterm (ID "<>" []) = "< >"
makeprettyterm (ID "<>" terms) = "<" ++ (makeprettyterms terms) ++ ">"
makeprettyterm (ID name []) = (makeprettyname name) ++ "()"
makeprettyterm (ID name terms) = (makeprettyname name) ++ "(" 
                                 ++ (makeprettyterms terms) ++ ")"

-- Print signature
makeprettysig :: [(Id, Int, Int)] -> String
makeprettysig [] = ""
makeprettysig ((id, i, o):sigs) =
  " (" ++ id ++ " " ++ show i ++ " " ++ show o ++ ")"
  ++ makeprettysig sigs

-- Print rule
makeprettyrules :: [Rule] -> String
makeprettyrules [] = ""
makeprettyrules ((Arrow lterm rterm []):rules) =
  "  " ++ (makeprettyterm lterm) ++ " -> " 
  ++ (makeprettyterm rterm) ++ "\n" ++ (makeprettyrules rules)
makeprettyrules ((Arrow lterm rterm conds):rules) =
  "  " ++ (makeprettyterm lterm) ++ " -> " 
  ++ (makeprettyterm rterm) ++ " <= " ++ (makeprettyconds conds) 
  ++ "\n" ++ (makeprettyrules rules)

-- Print CCS
makepretty :: CCS -> String
makepretty (Ccs {idlist = Var vars, funlist = Sig sigs,
                 rulelist = Rules rules}) =
   ("(VAR " ++ (filter (/='\"') (listToString vars " ")) ++ ")"
   ++ "\n"
   ++ "(SIG \n" ++ " " ++ (makeprettysig sigs) ++ ")"
   ++ "\n"
   ++ "(RULES \n" ++ (makeprettyrules rules) ++ ")\n")

------------------------
-- PRETTY DIAGNOSTICS --
------------------------
-- if True, mark with x
booltochar :: Bool -> Char
booltochar True = 'x'
booltochar False = ' '

-------------------------
-- GET POSITIONS/RULES --
-------------------------
-- These functions are not very nice - almost copied/pasted from 
-- file CCSProps.hs. Would have been nice with a general function:
-- getpropertypos :: Prop -> (Int, Int, Int, [Rule]) -> CCS 
--                        -> (Int, Int, Int, [Rule])
                   
-- Get line number and position of non-left-to-right-determnistic rules
getDetermpos :: (Int, Int, Int, [Rule]) -> CCS -> 
                (Int, Int, Int, [Rule])
getDetermpos (num, total, line, arule) 
             (Ccs {idlist = Var vars, 
                   funlist = Sig sigs,
                   rulelist = Rules []}) = (num, total, line, arule)
getDetermpos (num, total, line, arule) 
             (Ccs {idlist = Var vars, 
                   funlist = Sig sigs,
                   rulelist = Rules ((Arrow lterm rterm conds):rules)}) =
  let knownvar = getknownvar (Arrow lterm rterm conds)
  in case (isDeterm' (Arrow lterm rterm conds) knownvar) of
    True -> getDetermpos (num, total+1, line, arule) 
                         (Ccs {idlist = Var vars,
                               funlist = Sig sigs,
                               rulelist = Rules rules})
    False -> case line of
               0 -> getDetermpos (num + 1, total + 1, 
                                  line + 1 + total, 
                                  [(Arrow lterm rterm conds)])
                                 (Ccs {idlist = Var vars,
                                       funlist = Sig sigs,
                                       rulelist = Rules rules})
               _ -> getDetermpos (num + 1, total + 1, line, arule)
                                 (Ccs {idlist = Var vars,
                                       funlist = Sig sigs,
                                       rulelist = Rules rules})

-- Get line number and position of non-left-to-right-determnistic rules
getEVpos :: (Int, Int, Int, [Rule]) -> CCS -> (Int, Int, Int, [Rule])
getEVpos (num, total, line, arule) 
         (Ccs {idlist = Var vars, 
               funlist = Sig sigs,
               rulelist = Rules []}) = (num, total, line, arule)
getEVpos (num, total, line, arule) 
         (Ccs {idlist = Var vars, 
               funlist = Sig sigs,
               rulelist = Rules ((Arrow lterm rterm conds):rules)}) =
  case (isSubset (var [rterm]) 
                 (var [lterm] ++ (var (getterms conds)))) of
    True -> getEVpos (num, total+1, line, arule) 
                     (Ccs {idlist = Var vars,
                           funlist = Sig sigs,
                           rulelist = Rules rules})
    False -> case line of
               0 -> getEVpos (num + 1, total + 1, line + 1 + total,
                              [Arrow lterm rterm conds])
                             (Ccs {idlist = Var vars,
                                   funlist = Sig sigs,
                                   rulelist = Rules rules})
               _ -> getEVpos (num + 1, total + 1, line, arule)
                             (Ccs {idlist = Var vars,
                                   funlist = Sig sigs,
                                   rulelist = Rules rules})

getInvType :: CCS -> Maybe (String, String, String)
getInvType (Ccs {idlist = Var vars, 
                funlist = Sig sigs,
                rulelist = Rules ((Arrow (ID name lterms) 
                                       rterm conds):rules)}) =
 case parseFun name [] [] of
   Left err -> Nothing
--   Right maybetask -> 
--     case maybetask of
--       Nothing -> Nothing
--       Just (InvTask name' i o) -> 
   Right (InvTask name' i o) ->
         let invtype = case find (\(fun, iorig, oorig) -> fun == name') sigs of
                         Just (fun', iorig', oorig') ->
                           invType (length i + length o) 
                                   (iorig' + oorig' - 
                                    (length i + length o))
                                   iorig' oorig'
                         Nothing -> "Original program"
         in Just (listToString i ",", 
                  listToString o ",",
                  invtype)

------------------------
-- PRETTY DIAGNOSTICS --
------------------------
diagpretty :: FilePath -> CCS -> String
diagpretty path ccs =
  let diagnostics = makediagnostics ccs
      -- if not EV-free, find out where!
      evfreeStr = case (booltochar $ evFree diagnostics) of
                    'x' -> ""
                    ' ' -> 
                      let (num, numTotal, line, arule) = 
                            getEVpos (0,0,0,[]) ccs
                      in
                      "  ~ There are " ++ (show num) ++ 
                      " EVs out of " ++ (show numTotal) ++ 
                      " rules in total.\n" ++ 
                      "  ~ The first EV can be found in rule number " ++  
                      (show line) ++ "\n" ++
                      "  ~ The first rule with an EV is \n    " ++ 
                      (show arule) ++ "\n" ++ "\n" 

      -- if not ltr-deterministic, find out where!
      determStr = case (booltochar $ ltrDeterm diagnostics) of
                    'x' -> ""
                    ' ' -> 
                      let (num, numTotal, line, arule) = 
                            getDetermpos (0,0,0,[]) ccs
                      in
                      "  ~ There are " ++ (show num) ++ 
                      " non-left-to-right-deterministic rules out of " ++ 
                      (show numTotal) ++ " rules in total.\n" ++ 
                      "  ~ The first non-deterministic rule" ++ 
                      "can be found in rule " ++  
                      (show line) ++ "\n" ++
                      "  ~ The non-deterministic rule is \n    " ++ 
                      (show arule) ++ "\n" ++ "\n" 

      -- analyze inversion type
      invTypeStr = 
        case getInvType ccs of
          Nothing -> "-- Original program, not inverted\n"
          Just (i, o, invTask) -> "-- I = {" ++ i ++ "}, "
                                  ++ "O = {" ++ o ++ "}" ++
                                  ", " ++ invTask ++ "\n"
                                              
  in
  -- Time for printing!
  "------------------------------------------\n" ++
  "-- ANALYSING " ++ path ++ "\n" ++
  "------------------------------------------\n" ++
  invTypeStr ++
  "------------------------------------------\n" ++
  "Number of functions                          "
  ++ (show $ noOfFuns diagnostics) ++ "    \n" ++
  "Number of rules                              "
  ++ (show $ noOfRules diagnostics) ++ "    \n" ++
  "\n" ++ 
  "Functional                                   [" 
  ++ [booltochar $ functional diagnostics] ++ "]    \n" ++
  "- Orthogonal                                 [" 
  ++ [booltochar $ orthogonal diagnostics] ++ "]    \n" ++
  "  . Non-overlapping                          [" 
  ++ [booltochar $ nonOverlapping diagnostics] ++ "]    \n" ++
  "  . Left-linear                              [" 
  ++ [booltochar $ lLinear diagnostics] ++ "]    \n" ++
  "- EV-Free                                    [" 
  ++ [booltochar $ evFree diagnostics] ++ "]    \n" 
  ++ evfreeStr ++
  --"- Singular computational                       [" 
  -- ++ [booltochar $ ecFree diagnostics] ++ "]    \n" ++
  "- Left-to-right determ.                      [" 
  ++ [booltochar $ ltrDeterm diagnostics] ++ "]    \n" 
  ++ determStr ++
  "\n" ++
  "Reversible                                   [" 
  ++ [booltochar $ reversible diagnostics] ++ "]    \n" ++
  "- Functional                                 ["
  ++ [booltochar $ functional diagnostics] ++ "]    \n" ++
  "- Output-orthogonal                          ["
  ++ [booltochar $ outputOrthogonal diagnostics] ++ "]    \n" ++
  "  . Non-output-overlapping                   [" 
  ++ [booltochar $ nonOutputOverlapping diagnostics] ++ "]    \n" ++
  "  . Right-linear                             [" 
  ++ [booltochar $ rLinear diagnostics] ++ "]    \n" ++
  "- Strictly non-erasing                       [" 
  ++ [booltochar $ strictNonErasing diagnostics] ++ "]    \n" ++
  "  . Non-erasing                              [" 
  ++ [booltochar $ nonErasing diagnostics] ++ "]    \n" ++
  "  . Weakly non-erasing                       [" 
  ++ [booltochar $ weakNonErasing diagnostics] ++ "]    \n" 

-------------------------------------
-- PRETTY DIAGNOSTICS - COMPARISON --
-------------------------------------
comppretty :: FilePath -> FilePath -> CCS -> CCS -> String
comppretty path pathOrig ccs ccsOrig =
  let diag1 = makediagnostics ccs
      diag2 = makediagnostics ccsOrig
      spacesRules = 7 - ((noOfRules diag1) `div` 10)
      spacesFuns = 7 - ((noOfFuns diag1) `div` 10)
  in
  "----------------------------------\n" ++
  "-- COMPARING " ++ path ++ " (CCS1) with " ++ pathOrig ++ 
  " (CCS2)" ++ "\n" ++
  "----------------------------------\n" ++
  "                                     CCS1    CCS2 \n" ++
  "Number of functions                  "
  ++ (show $ noOfFuns diag1) ++ (replicate spacesFuns ' ') 
  ++ (show $ noOfFuns diag2) ++ "\n" ++
  "Number of rules                      "
  ++ (show $ noOfRules diag1) ++ (replicate spacesRules ' ') 
  ++ (show $ noOfRules diag2) ++ "\n" ++
  "\n" ++
  "Functional                           [" 
  ++ [booltochar $ functional diag1]  ++ "]     [" 
  ++ [booltochar $ functional diag2] ++ "]    \n" ++
  "- Orthogonal                         [" 
  ++ [booltochar $ orthogonal diag1]       ++ "]     [" 
  ++ [booltochar $ orthogonal diag2] ++ "]    \n" ++
  "  . Non-overlapping                  [" 
  ++ [booltochar $ nonOverlapping diag1]       ++ "]     [" 
  ++ [booltochar $ nonOverlapping diag2] ++ "]    \n" ++
  "  . Left-linear                      [" 
  ++ [booltochar $ lLinear diag1]        ++ "]     [" 
  ++ [booltochar $ lLinear diag2] ++ "]    \n" ++
  "- EV-Free                            [" 
  ++ [booltochar $ evFree diag1]      ++ "]     [" 
  ++ [booltochar $ evFree diag2] ++ "]    \n" ++
--  "- Singular computational             [" 
--  ++ [booltochar $ ecFree diag1]      ++ "]     [" 
--  ++ [booltochar $ ecFree diag2] ++ "]    \n" ++
  "- Left-to-right determ.              [" 
  ++ [booltochar $ ltrDeterm diag1]   ++ "]     [" 
  ++ [booltochar $ ltrDeterm diag2] ++ "]    \n" ++
  "\n" ++
  "Reversible                           [" 
  ++ [booltochar $ reversible diag1]  ++ "]     [" 
  ++ [booltochar $ reversible diag2] ++ "]    \n" ++
  "- Functional                         [" 
  ++ [booltochar $ functional diag1]     ++ "]     [" 
  ++ [booltochar $ functional diag2] ++ "]    \n" ++
  "- Output-orthogonal                  [" 
  ++ [booltochar $ outputOrthogonal diag1]     ++ "]     [" 
  ++ [booltochar $ outputOrthogonal diag2] ++ "]    \n" ++
  "  . Non-output-overlapping           [" 
  ++ [booltochar $ nonOutputOverlapping diag1]     ++ "]     [" 
  ++ [booltochar $ nonOutputOverlapping diag2] ++ "]    \n" ++
  "  . Right-linear                     [" 
  ++ [booltochar $ rLinear diag1]        ++ "]     [" 
  ++ [booltochar $ rLinear diag2] ++ "]    \n" ++
  "- Strictly non-erasing               [" 
  ++ [booltochar $ strictNonErasing diag1] ++ "]     [" 
  ++ [booltochar $ strictNonErasing diag2] ++ "]    \n" ++
  "  . Non-erasing                      [" 
  ++ [booltochar $ nonErasing diag1]       ++ "]     [" 
  ++ [booltochar $ nonErasing diag2] ++ "]    \n" ++
  "  . Weakly non-erasing               [" 
  ++ [booltochar $ weakNonErasing diag1]   ++ "]     [" 
  ++ [booltochar $ weakNonErasing diag2] ++ "]    \n"

