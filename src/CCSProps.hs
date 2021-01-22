module CCSProps where

import CCSAst
import CCSParser
import Inverter
--import CCSPretty

import Data.List

-- A "Property" is a function, which takes a CCS and returns a
-- boolean value
type Prop = (CCS -> [Id] -> Bool)

-- Diagnostics are a collection of properties' absence or presence
data Diagnostics = Analysis {noOfRules            :: Int, 
                             noOfFuns             :: Int,
                             evFree               :: Bool,
                             ltrDeterm            :: Bool,
                             nonErasing           :: Bool,
                             weakNonErasing       :: Bool,
                             strictNonErasing     :: Bool,
                             lLinear              :: Bool,
                             rLinear              :: Bool,
                             nonOverlapping       :: Bool,
                             nonOutputOverlapping :: Bool,
                             orthogonal           :: Bool,
                             outputOrthogonal     :: Bool,
                             ecFree               :: Bool,
                             functional           :: Bool,
                             reversible           :: Bool}

-- collect diagnostics
makediagnostics :: CCS -> Diagnostics
makediagnostics ccs =
  Analysis {noOfRules            = numberofrules ccs,
            noOfFuns             = numberoffuns ccs [],
            evFree               = isEvFree ccs,
            ltrDeterm            = isDeterm ccs,
            nonErasing           = isNonerasing ccs,
            weakNonErasing       = isWeaknonerasing ccs,
            strictNonErasing     = isNonerasing ccs && 
                                   isWeaknonerasing ccs,
            lLinear              = isLeftlinearCCS ccs,
            rLinear              = isRightlinearCCS ccs,
            nonOverlapping       = isNonOverlappingCCS ccs ccs,
            nonOutputOverlapping = isNonOutputOverlappingCCS ccs ccs,
            orthogonal           = isOrthogonal ccs,
            outputOrthogonal     = isOpOrthogonal ccs,
            ecFree               = isEcFree ccs,
            functional           = isOrthogonal ccs && 
                                   isEvFree ccs && isDeterm ccs,
            reversible           = isOrthogonal ccs && 
                                   isEvFree ccs && isDeterm ccs && 
                                   isOpOrthogonal ccs &&
                                   isNonerasing ccs && 
                                   isWeaknonerasing ccs
           }

--------------------------
-- DESIRABLE PROPERTIES --
--------------------------
-- 1. EV-free --
----------------
getterms :: [Cond] -> [Term]
getterms [] =  []
getterms ((CArrow lcond rcond):conds) = (lcond:rcond:(getterms conds))

getleftterms :: [Cond] -> [Term]
getleftterms [] =  []
getleftterms ((CArrow lcond rcond):conds) = (lcond:(getleftterms conds))

getrightterms :: [Cond] -> [Term]
getrightterms [] =  []
getrightterms ((CArrow lcond rcond):conds) = (rcond:(getrightterms conds))

isEvFree :: CCS -> Bool
-- no more rules. It is EV-free!
isEvFree (Ccs {idlist = Var vars, funlist = sigs, 
               rulelist = Rules []}) = True
-- is var(r) subset var(l, c)?
-- if not, it is not EV-free
isEvFree (Ccs {idlist = Var vars, funlist = sigs,
               rulelist = Rules ((Arrow lterm rterm conds):rules)}) =
   case (isSubset (var [rterm]) (var [lterm] ++ (var (getterms conds)))) of
     True -> (isEvFree (Ccs {idlist = Var vars, 
                             funlist = sigs,
                             rulelist = Rules rules}))
     False -> False

-------------------------------
-- 2. Singular Computational --
-------------------------------
-- or "extra computation free"
isEcFreeRule :: Rule -> Bool
isEcFreeRule (Arrow lterm rterm []) = True
isEcFreeRule (Arrow lterm rterm ((CArrow lcterm rcterm):conds)) =
  case (intersect (var [rcterm]) (var $ getrightterms conds)) of
    []     -> isEcFreeRule (Arrow lterm rterm conds)
    (x:xs) -> False

isEcFree :: CCS -> Bool
-- no more rules. It is EV-free!
isEcFree (Ccs {idlist = Var vars, funlist = sigs, 
               rulelist = Rules []}) = True
-- is var(r) subset var(l, c)?
-- if not, it is not EV-free
isEcFree (Ccs {idlist = Var vars, funlist = sigs,
               rulelist = Rules (rule:rules)}) =
   case isEcFreeRule rule of
     True -> (isEcFree (Ccs {idlist = Var vars, 
                             funlist = sigs,
                             rulelist = Rules rules}))
     False -> False

------------------------------------
-- 3. Left-to-right deterministic --
------------------------------------
getknownvar :: Rule -> [Id]
getknownvar (Arrow lterm rterm []) = var [lterm]
getknownvar (Arrow lterm rterm ((CArrow lcond rcond):[])) = var [lterm] ++ var [rcond]
getknownvar (Arrow lterm rterm ((CArrow lcond rcond):conds)) = 
  (var [lterm]) ++ (var [rcond]) ++ 
  (getknownvar (Arrow (ID "" []) (ID "" []) conds))

isDeterm :: CCS -> Bool
isDeterm (Ccs {idlist = Var vars, 
               funlist = sigs,
               rulelist = Rules []}) = True
isDeterm (Ccs {idlist = Var vars, 
               funlist = sigs,
               rulelist = Rules ((Arrow lterm rterm conds):rules)}) =
  let knownvar = getknownvar (Arrow lterm rterm conds)
  in isDeterm' (Arrow lterm rterm conds) knownvar &&
     isDeterm (Ccs {idlist = Var vars, 
                    funlist = sigs,
                    rulelist = Rules rules})
 
isDeterm' :: Rule -> [Id] -> Bool
isDeterm' (Arrow lterm rterm []) knownvar = True
isDeterm' (Arrow lterm rterm ((CArrow lcond rcond):conds)) knownvar =
  (isSubset (var [lcond]) knownvar) &&
   isDeterm' (Arrow lterm rterm conds) knownvar

---------------
-- 4. Linear --
---------------
unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = case x `elem` xs of
                  True -> False
                  False -> unique xs 

isLinear :: Term -> Bool
isLinear (ID name terms) = let vars = var terms
                           in unique vars

-- 4.1. Left-linear
isLeftlinear :: Rule -> Bool
isLeftlinear (Arrow lterm rterm conds) = isLinear lterm

isLeftlinearCCS :: CCS -> Bool
isLeftlinearCCS (Ccs {idlist = Var vars, 
                      funlist = sigs,
                      rulelist = Rules rules}) =
  all (\rule -> isLeftlinear rule) rules

-- 4.2. Right-linear
isRightlinear :: Rule -> Bool
isRightlinear (Arrow lterm rterm conds) = isLinear rterm

isRightlinearCCS :: CCS -> Bool
isRightlinearCCS (Ccs {idlist = Var vars, 
                       funlist = sigs,
                       rulelist = Rules rules}) =
  all (\rule -> isRightlinear rule) rules

-------------
-- 5. Size --
-------------
-- count rules and funs
numberoffuns :: CCS -> [Id] -> Int
numberoffuns (Ccs {idlist = Var vars, 
                    funlist = sigs,
                    rulelist = Rules []}) seen = 0
numberoffuns (Ccs {idlist = Var vars, 
                    funlist = sigs,
                    rulelist = Rules ((Arrow (ID name lterms) 
                                       rterm conds):rules)}) seen =
  case name `elem` seen of
    False -> 1 + numberoffuns (Ccs {idlist = Var vars,
                                     funlist = sigs,
                                     rulelist = Rules rules}) (name:seen)
    True -> numberoffuns (Ccs {idlist = Var vars,
                                funlist = sigs,
                                rulelist = Rules rules}) (name:seen)

numberofrules :: CCS -> Int
numberofrules (Ccs {idlist = Var vars, 
                    funlist = sigs,
                    rulelist = Rules rules}) = length rules

----------------
-- 6. Erasing --
----------------
-- 6.1 - non-erasing
isNonerasing :: CCS -> Bool
isNonerasing (Ccs {idlist = Var vars, 
                   funlist = sigs,
                   rulelist = Rules []}) = True
isNonerasing (Ccs {idlist = Var vars, 
                   funlist = sigs,
                   rulelist = Rules ((Arrow lterm rterm conds):rules)}) =
  (isSubset (var [lterm]) (var ([rterm]++(getterms conds))))
   && isNonerasing (Ccs {idlist = Var vars,
                         funlist = sigs,
                         rulelist = Rules rules})

-- 6.2 weakly non-erasing
isWeaknonerasing :: CCS -> Bool
isWeaknonerasing (Ccs {idlist = Var vars, 
                       funlist = sigs,
                       rulelist = Rules []}) = True
isWeaknonerasing (Ccs {idlist = Var vars, 
                       funlist = sigs,
                       rulelist = Rules ((Arrow lterm rterm 
                                          []):rules)}) =
  isWeaknonerasing (Ccs {idlist = Var vars,
                         funlist = sigs,
                         rulelist = Rules rules})
isWeaknonerasing (Ccs {idlist = Var vars, 
                       funlist = sigs,
                       rulelist = Rules ((Arrow lterm rterm 
                                         ((CArrow lcond rcond):conds)):rules)}) =
  (isSubset (var [rcond]) (var ([rterm]++[lcond]++(getleftterms conds))))
  && isWeaknonerasing (Ccs {idlist = Var vars,
                            funlist = sigs,
                            rulelist = Rules ((Arrow lterm rterm
                                               conds):rules)})

--------------------
-- 7. Overlapping --
--------------------
occurs :: Id -> [Term] -> Bool
occurs var [] = False
occurs var ((VarID var' []):terms) = var == var' || occurs var terms
occurs var ((ID name terms):terms') = occurs var terms || occurs var terms'

-- eliminate var from lookup table
varElim :: Term -> Term -> [(Id, Term)] -> Maybe [(Id, Term)]
varElim (VarID var []) (ID name terms) table =
  case lookup var table of
    Nothing -> Just table
    Just (ID name' terms') -> if name == name' then Just table
                              else mgu (ID name terms) 
                                       (ID name' terms') table

-- following a MGU algorithm
mguterms :: [Term] -> [Term] -> [(Id, Term)] -> Maybe [(Id, Term)]
mguterms [] [] table = Just table
mguterms [] terms _ = Nothing
mguterms terms [] _ = Nothing
mguterms (term:terms) (term':terms') table
  | (length terms == length terms') = 
      case mgu term term' table of
        Nothing -> Nothing
        Just binding -> 
          case mguterms terms terms' binding of
            Nothing -> Nothing
            Just bindings -> Just (binding ++ bindings)
  | otherwise = Nothing 

-- mgu returns a list of bindings
mgu :: Term -> Term -> [(Id, Term)] -> Maybe [(Id, Term)]
mgu (VarID var []) (VarID var' []) table =
  case (lookup var table, lookup var' table) of
    (Nothing, Nothing) -> Just table 
    (Just (ID name terms), Nothing) -> 
      case occurs var' terms of
        True -> Nothing
        False -> Just table
    (Nothing, Just (ID name terms)) -> 
      case occurs var terms of
        True -> Nothing
        False -> Just table
    (Just (ID name terms), Just (ID name' terms')) -> 
      case (occurs var' terms, occurs var terms') of
        (False, False) -> Just table
        _ -> Just []
        
mgu (VarID var []) (ID name terms) table =
  case occurs var terms of
    True -> Nothing
    False -> varElim (VarID var []) (ID name terms) table
mgu (ID name terms) (VarID var []) table = 
  case occurs var terms of
    True -> Nothing
    False -> Just ((var, (ID name terms)):table)
mgu (ID name terms) (ID name' terms') table = 
  if name == name' 
  then mguterms terms terms' table
  else Nothing
mgu _ _ _ = Nothing

overlap :: Term -> Term -> Bool
overlap term term' = case mgu term term' [] of
                       Nothing -> False
                       Just a -> True

-- two rules are overlapping if l and l' are overlapping
isOverlapping :: Rule -> Rule -> Bool
isOverlapping (Arrow lterm rterm conds) (Arrow lterm' rterm' conds') =
  overlap lterm lterm'

-- two rules are output overlapping if root(l) == root(l')
-- and r and r' overlap
isOpOverlapping :: Rule -> Rule -> Bool
isOpOverlapping (Arrow (ID name lterms) rterm conds) 
                (Arrow (ID name' lterm') rterm' conds') =
  (name == name') && (overlap rterm rterm')
   
-- third arg: saving original
isNonOverlappingCCS :: CCS -> CCS -> Bool
isNonOverlappingCCS (Ccs {idlist = Var vars,
                          funlist = sigs,
                          rulelist = Rules rules})
                    (Ccs {idlist = Var vars',
                          funlist = sigs',
                          rulelist = Rules rules'}) =
  let rulesProd = [(rule, rule') | rule <- rules, 
                                   rule' <- rules', rule /= rule']
  -- are there any overlapping rules? Negate this boolean value
  in not(any (\comb -> isOverlapping (fst comb) (snd comb)) rulesProd)

isNonOutputOverlappingCCS :: CCS -> CCS -> Bool
isNonOutputOverlappingCCS (Ccs {idlist = Var vars,
                                funlist = sigs,
                                rulelist = Rules rules})
                          (Ccs {idlist = Var vars',
                                funlist = sigs',
                                rulelist = Rules rules'}) =
  let rulesProd = [(rule, rule') | rule <- rules, 
                                   rule' <- rules', rule /= rule']
  -- are there any output-overlapping rules? Negate this boolean value
  in not(any (\comb -> isOpOverlapping (fst comb) (snd comb)) rulesProd)

--------------------
-- 8. Orthogonal  --
--------------------
isOrthogonal :: CCS -> Bool
isOrthogonal ccs =
  (isNonOverlappingCCS ccs ccs) && (isLeftlinearCCS ccs)

isOpOrthogonal :: CCS -> Bool
isOpOrthogonal ccs =
  (isNonOutputOverlappingCCS ccs ccs) && (isRightlinearCCS ccs)
