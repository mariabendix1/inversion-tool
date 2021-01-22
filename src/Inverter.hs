module Inverter where

import CCSAst
import CCSParser

import Data.Either
import Data.List
import Data.Ord


filterTerms :: [Term] -> [Int] -> Bool -> [Term]
filterTerms termlist idxs negate
  -- this is not computationally effective at all :-)
  -- filters/"unfilters" from io-sets
  | negate = [x | (n,x) <- zip [0..] termlist, not(n `elem` idxs)]
  | otherwise = [x | (n,x) <- zip [0..] termlist, (n `elem` idxs)]

-- Add IO-sets to condition list
addIO :: [Cond] -> [Cond]
addIO [] = []
addIO ((CArrow (ID fun termlist_l) (ID brc termlist_r)):conds) =
  let i = case (length termlist_l) of
            0 -> []
            1 -> [0]
            x -> [0..(x-1)]
  in ((CArrow (ID (fun++"{"++(listToString i ",")++"}{}") termlist_l) (ID brc termlist_r)):(addIO conds))
--    in ((CArrow (ID (fun++"{0,1}{}") termlist_l) (ID brc termlist_r)):conds)

sigLookup :: Id -> [(Id, Int, Int)] -> Maybe (Int, Int)
sigLookup name [] = Nothing
sigLookup name ((name', arity, coarity):xs) =
  if name == name' then Just (arity, coarity) else (sigLookup name xs)

makeNameI :: [(Int, String)] -> [(Int, String)] -> [Int] -> [Int] -> [(Int, String)]
makeNameI orig_i orig_o i o = [orig_i!!index | index <- i] ++ [orig_o!!index | index <- o]

makeNameO :: [(Int, String)] -> [(Int, String)] -> [Int] -> [Int] -> [(Int, String)]
makeNameO orig_i orig_o i o = (orig_i ++ orig_o) \\ ([orig_i!!index | index <- i] ++ [orig_o!!index | index <- o])

makeNewI :: [(Int, String)] -> [(Int, String)] -> [Int] -> [Int] -> [(Int, String)]
makeNewI orig_i orig_o i o = [orig_i!!index | index <- i] ++ [orig_o!!index | index <- o]

makeNewO :: [(Int, String)] -> [(Int, String)] -> [Int] -> [Int] -> [(Int, String)]
makeNewO orig_i orig_o i o = (orig_i ++ orig_o) \\ ([orig_i!!index | index <- i] ++ [orig_o!!index | index <- o])

makeResI :: [(Int, String)] -> [Int]
makeResI []               = []
makeResI ((n,"i"):(rest)) = ((n - 1):(makeResI rest))
makeResI ((n,str):(rest)) = (makeResI rest)  

makeResO :: [(Int, String)] -> [Int]
makeResO []               = []
makeResO ((n,"o"):(rest)) = ((n - 1):(makeResO rest))  
makeResO ((n,str):(rest)) = (makeResO rest)  

makeOrig :: Int -> String -> Int -> [(Int, String)]
makeOrig 0 s n = []
makeOrig x s n = ((n,s):(makeOrig (x-1) s (n+1))) 

-- Analyze whether the function was already inverted
-- outputs new I-set, 
makeNewIO :: Id -> [(Id, Int, Int)] -> [Int] -> [Int] -> ([Int], [Int])
makeNewIO fun sig is os = 
  (is, os)
--  case parseFun fun [] [] of
--    Left err -> ([],[])
--    Right (InvTask name [] []) -> (is, os)
--    Right (InvTask name i o) -> 
--      case (sigLookup name sig) of
--        Nothing -> (is, os)
--                                 -- from 1-index to 0-index
--        Just (arity, coarity) -> let ii = map (\x -> x-1) i
--                                     oo = map (\x -> x-1) o
--                                     orig_i = makeOrig arity "i" 1
--                                     orig_o = makeOrig coarity "o" 1
--                                     name_i = makeNameI orig_i orig_o ii oo
--                                     name_o = makeNameO orig_i orig_o ii oo
--                                     new_i  = makeNewI name_i name_o is os
--                                     new_o  = makeNewO name_i name_o is os
--                                     res_i  = makeResI new_i
--                                     res_o  = makeResO new_i
--                                 in (res_i,res_o)

---------------------
-- TRIVIAL RULEINV --
---------------------
ruleinvTriv :: Rule -> [(Id, Int, Int)] -> 
               [Int] -> [Int] -> Maybe Rule
ruleinvTriv (Arrow (ID fun termlist_l) (ID ang termlist_r) condlist) 
            sig i o
  -- guard: only evaluate if I and O are "in range"
  | (all (< length termlist_l) i) && (all (< length termlist_r) o) = 
      -- include io-sets {n} {m} in names
      let (i_new, o_new) = makeNewIO fun sig i o
          fun2           = case (isInfixOf "{" fun) of
                                True -> case (isInfixOf "_" fun) of
                                          False -> fun ++ "_"
                                          True -> fun ++ "_"
                                False -> fun
      in
      Just (Arrow (ID (fun++"{"++(listToString i_new ",")++"}{"
                          ++(listToString o_new ",")++"}") 
                      (termlist_l_filter ++ termlist_r_filter))
                  (ID ang (termlist_l_negfilter ++ termlist_r_negfilter))
                  condlist_io)

  -- otherwise, nothing
  | otherwise = Nothing
        -- exctract elements from indices
  where termlist_r_filter = filterTerms termlist_r o False
        termlist_l_filter = filterTerms termlist_l i False
        -- remove elements from indices
        termlist_r_negfilter = filterTerms termlist_r o True
        termlist_l_negfilter = filterTerms termlist_l i True
        condlist_io = addIO condlist   

------------------------
-- LOCALINV PURE FULL --
------------------------
-- make list from 0...m
makeOset :: [Term] -> Int -> [Int]
makeOset l i = case l of
  [] -> []
  (x:xs) -> i:(makeOset xs (i+1))

-- mapswap (from Kirkeby, Glueck)
mapswap :: [Cond] -> [(Id, Int, Int)] -> Either ErrMsg [Cond]
mapswap [] sig = Right []
mapswap ((CArrow lcond (ID "<>" rcond)):conds) sig = 
  -- invert fully
  --                                                sig
  case ruleinvTriv (Arrow lcond (ID "<>" rcond) []) sig  [] 
                   (makeOset rcond 0) of
    Just (Arrow lcond' rcond' []) -> 
      case (mapswap conds sig) of
        Right conds' -> Right ([CArrow lcond' rcond'] ++ conds')
        Left err -> Left err
    Nothing -> Left "mapswap failed"

-----------------------
-- PURE FULL RULEINV --
-----------------------
ruleinvPfull :: Rule -> [(Id, Int, Int)] ->
                [Int] -> [Int] -> Maybe Rule
ruleinvPfull (Arrow (ID fun termlist_l) 
                    (ID ang termlist_r) condlist) sig i o
  -- guard: only evaluate if I and O are "in range"
  | (all (< length termlist_l) i) && (all (< length termlist_r) o) = 
         let (i_new, o_new) = makeNewIO fun sig i o
             fun2           = case (isInfixOf "{" fun) of
                                True -> case (isInfixOf "_" fun) of
                                          False -> fun ++ "_"
                                          True -> fun
                                False -> fun
         in
         let l' = (ID (fun++"{"++(listToString i_new ",")++"}{"
                          ++(listToString o_new ",")++"}")
                      (termlist_l_filter ++ termlist_r_filter))
             r' = (ID ang (termlist_l_negfilter ++ termlist_r_negfilter))
         -- reverse
             c' = reverse condlist
         -- map swap
         in case (mapswap c' sig) of
           Left err -> Nothing
           Right c'' -> Just (Arrow l' r' c'')

  -- otherwise, nothing
  | otherwise = Nothing
        -- exctract elements from indices
  where termlist_r_filter = filterTerms termlist_r o False
        termlist_l_filter = filterTerms termlist_l i False
        -- remove elements from indices
        termlist_r_negfilter = filterTerms termlist_r o True
        termlist_l_negfilter = filterTerms termlist_l i True

--------------------------
-- PURE PARTIAL RULEINV --
--------------------------
---------
-- VAR -- 
---------
-- get known variables from terms
var :: [Term] -> [Id]
var [] = []
var ((ID name rest):terms) = (var rest) ++ (var terms)
var ((VarID name rest):terms) = (name: (var rest) ++ (var terms))

---------------------
-- I O set helpers -- 
---------------------
-- subset - used for checking subsets of knownvar below
isSubset :: (Ord a, Eq a) => [a] -> [a] -> Bool
isSubset [] _ = True
isSubset _ [] = False
isSubset ids ids' =
  -- might not be sorted, so
  isSubset' (sort (nub ids)) (sort (nub ids'))

isSubset' :: (Ord a, Eq a) => [a] -> [a] -> Bool
isSubset' [] _ = True
isSubset' _ [] = False
isSubset' (id:ids) (id':ids')
  | id == id' = isSubset ids ids'
  | otherwise = isSubset (id:ids) ids'
 
-- I := {i | pi in pn, var(pi) subset knownvar}
makeset :: [Term] -> [Id] -> Int -> [Int]
makeset [] knownvar i = []
makeset (term:terms) knownvar i = 
  case isSubset (var [term]) knownvar of
    True -> i:(makeset terms knownvar (i+1))
    False -> (makeset terms knownvar (i+1))

--------------------------
-- LOCALINV PART + SEMI --
--------------------------
--          conds   knownvar  semi/part
localinv :: [Cond] -> [Id] -> String -> [Cond]
localinv [] knownvar semipart = []
localinv ((CArrow (ID name lterms) (ID "<>" rterms)):conds) 
         knownvar semipart =
  let c' = []
  in let i = makeset lterms knownvar 0
  in let o = case semipart of
               "semi" -> makeset rterms knownvar 0
                -- include everything
               "part" -> makeOset rterms 0
               _ -> []
  in case ruleinvTriv (Arrow (ID name lterms) 
--                                                sig 
                             (ID "<>" rterms) []) []  i o of
       Just (Arrow lterm' rterm' []) -> 
          let knownvar' = knownvar ++ (var [lterm', rterm'])
          in ((CArrow lterm' rterm'):(localinv conds knownvar' semipart))
       Nothing -> [] -- change

-------------------------------
-- PARTIAL INVERSION RULEINV --
-------------------------------
ruleinvPart :: Rule -> [(Id, Int, Int)] ->
               [Int] -> [Int] -> Maybe Rule
ruleinvPart (Arrow (ID fun termlist_l) (ID ang termlist_r) condlist) 
            sig i o
  -- guard: only evaluate if I and O are "in range"
  | (all (< length termlist_l) i) && (all (< length termlist_r) o) = 
         let fun2           = case (isInfixOf "{" fun) of
                                True -> case (isInfixOf "_" fun) of
                                          False -> fun ++ "_"
                                          True -> fun
                                False -> fun
             l' = (ID (fun++"{"++(listToString i ",")++"}{"
                       ++(listToString o ",")++"}") 
                   (termlist_l_filter ++ termlist_r_filter))
             r' = (ID ang (termlist_l_negfilter ++ termlist_r_negfilter))
         -- reverse
             c' = reverse condlist
         -- localinv
             c'' = localinv c' (var [l']) "part"
         in Just (Arrow l' r' c'')  
  -- otherwise, nothing
  | otherwise = Nothing
        -- exctract elements from indices
  where termlist_r_filter = filterTerms termlist_r o False
        termlist_l_filter = filterTerms termlist_l i False
        -- remove elements from indices
        termlist_r_negfilter = filterTerms termlist_r o True
        termlist_l_negfilter = filterTerms termlist_l i True

----------------------------
-- SEMI INVERSION RULEINV --
----------------------------
percent :: [Id] -> Cond -> Float
percent knownvar (CArrow (ID name lcond) (ID "<>" rcond)) =
  let i = makeset lcond knownvar 0
      o = makeset rcond knownvar 0
  in (fromIntegral (length i + length o)) /
     (fromIntegral (length lcond + length rcond))

-- heuristic from Kirkeby and Glueck
heuristic :: [Cond] -> [Id] -> [Cond] 
heuristic [] knownvar = []
heuristic condlist knownvar =
  let ps = zip (map (percent knownvar) condlist) (zip condlist [0..])
      maxp = maximumBy (comparing fst) ps
      (maxcond, maxidx) = snd maxp
  in (maxcond:(heuristic (take maxidx condlist 
                          ++ drop (1 + maxidx) condlist) knownvar))

ruleinvSemi :: Rule -> [(Id, Int, Int)] -> 
               [Int] -> [Int] -> Maybe Rule
ruleinvSemi (Arrow (ID fun termlist_l) (ID ang termlist_r) condlist) 
            sig i o
  -- guard: only evaluate if I and O are "in range"
  | (all (< length termlist_l) i) && (all (< length termlist_r) o) = 
         let fun2           = case (isInfixOf "{" fun) of
                                True -> case (isInfixOf "_" fun) of
                                          False -> fun ++ "_"
                                          True -> fun
                                False -> fun
             l' = (ID (fun++"{"++(listToString i ",")++"}{"
                       ++(listToString o ",")++"}") 
                   (termlist_l_filter ++ termlist_r_filter))
             r' = (ID ang (termlist_l_negfilter ++ termlist_r_negfilter))
         -- reverse
             c' = heuristic condlist (var [l'])
         -- localinv
             c'' = localinv c' (var [l']) "semi"
         in Just (Arrow l' r' c'')  
  -- otherwise, nothing
  | otherwise = Nothing
        -- exctract elements from indices
  where termlist_r_filter = filterTerms termlist_r o False
        termlist_l_filter = filterTerms termlist_l i False
        -- remove elements from indices
        termlist_r_negfilter = filterTerms termlist_r o True
        termlist_l_negfilter = filterTerms termlist_l i True

------------------------------
-- MAIN INVERSION ALGORITHM --
------------------------------
-- get original i-o sets
count :: [Term] -> [Int]
count terms = [0..((length terms) - 1)]

-- get dependencies (this is really ugly oops)
getdep :: Rule -> [Task]
getdep (Arrow lterm rterm []) = []
getdep (Arrow lterm rterm ((CArrow (ID name lcond) 
                                   (ID brac rcond)):conds)) =
  let iorig = count lcond
      oorig = []
  in
  case parseFun name iorig oorig of
    --Left err -> []
    Left err -> [(InvTask "prut" [1] [1])]
    Right (InvTask f i o) -> let i_new = map (\x -> x - 0) i
                                 o_new = map (\x -> x - 0) o
                             in 
                             if (i == iorig && o == oorig) then ((InvTask f iorig oorig):(getdep (Arrow lterm rterm conds)))
                                                           else ((InvTask f i_new o_new):(getdep (Arrow lterm rterm conds))) 
--    Right maybetask -> 
--      case maybetask of
--        Nothing -> []
--        Just task -> task:(getdep (Arrow lterm rterm conds))

-- update siglist
makesig :: Id -> [Term] -> Term -> [(Id, Int, Int)] -> [(Id, Int, Int)]
makesig sig lterm (ID "<>" rterm) sigs =
  case find (== '}') sig of
    Just char -> sigs
    Nothing -> case find (\(sig', i, o) -> sig' == sig) sigs of 
                 Just s -> sigs
                 -- the new siglist goes to the back
                 Nothing -> (sigs ++ [(sig, length lterm, length rterm)])

-- update original ccs wrt signature list
updateorig :: CCS -> [(Id, Int, Int)] -> CCS
updateorig (Ccs {idlist = Var vars,
                 funlist = Sig sigs,
                 rulelist = Rules rules}) sigs' =
  -- remove duplicates from sigs'
  let sigs'' = filter (\(sig, i, o) -> 
                        not(sig `elem` (map (\(f, i, o) -> f) sigs))) sigs'
  in
  (Ccs {idlist = Var vars,
        funlist = Sig (sigs ++ sigs''),
        rulelist = Rules rules})

-- the tasklist here represents the pending set in the paper
-- the rulelist here represents r' in the paper
inv :: (Rule -> [(Id, Int, Int)] -> [Int] -> [Int] -> Maybe Rule) 
       -- pend      ccs    ccs    done      r'
       -> [Task] -> CCS -> CCS -> [Task] -> [Rule] 
       -> Either ErrMsg CCS
-- Pend == Ø, return R'
inv ruleinv [] (Ccs {idlist = Var vars, 
                     funlist = Sig sigs,
                     rulelist = Rules rules}) 
    orig done r' = 
  Right (Ccs {idlist = Var vars, 
              funlist = Sig sigs,
              rulelist = Rules r'})

-- while (Pend != Ø)
inv ruleinv ((InvTask f i o):pends) 
            (Ccs {idlist = Var vars, 
                  funlist = Sig sigs,
                  rulelist = Rules rules}) orig done r' = 
    -- invert all rules of f in R
    case rules of
      -- no more rules, look at next task, update done set
      -- start from the beginning when looking through rules
      [] -> inv ruleinv pends 
                (updateorig orig sigs) orig
                ((InvTask f i o):done) r'
  
      -- rule id(list) -> <...> <= conds 
      (Arrow (ID id list) right conds):rules' 
         -- apply ruleinv if not in done
         -> let idroot = case (parseFunDouble id [] []) of
                           Left err -> id
                           Right (InvTask froot i o) -> froot
            in
            --if ((id == f) && (not((InvTask id i o) `elem` done)))
            if ((id == f) && (not((InvTask id i o) `elem` done) && (not((InvTask idroot i o) `elem` done))))
            --then case (ruleinv (Arrow (ID id list) right conds) 
            then case (ruleinv (Arrow (ID idroot list) right conds) 
                       sigs i o) of
                   Nothing -> Left "The rule inverter failed. This may be because the given I/O-set is out of range."
                   Just rfio -> 
                     inv ruleinv (((InvTask f i o):pends) 
                                  ++ (getdep rfio)) 
                         (Ccs {idlist = Var vars, 
                               funlist = Sig (makesig f list right sigs),
                               rulelist = Rules rules'}) 
                         orig done (r' ++ [rfio]) 
            else inv ruleinv ((InvTask f i o):pends) 
                     (Ccs {idlist = Var vars, 
                           funlist = Sig sigs,
                           rulelist = Rules rules'}) 
                     orig done r'
