module Main where

import CCSAst
import CCSParser
import Inverter
import CCSProps

import Data.Typeable
import Text.Read
import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "Unit tests" 
  [testGroup "Abstract syntax"
  [testCase "Dummy add" $ 
    Ccs (Var ["x", "y", "z"]) 
        (Sig [])
        (Rules [(Arrow (ID "add" [ID "0" [], ID "y" []]) 
                       (ID "y" []) []), 
                (Arrow (ID "add" [ID "s" [ID "x" []], ID "y" []])
                       (ID "s" [ID "z" [], ID "y" []]) 
                [CArrow (ID "add" [ID "x" [], ID "y" []]) 
                        (ID "z" [])])])
    @?= Ccs {idlist   = Var ["x", "y", "z"], 
             funlist  = Sig [],
             rulelist = Rules [(Arrow (ID "add" [ID "0" [], ID "y" []]) 
                       (ID "y" []) []), 
                (Arrow (ID "add" [ID "s" [ID "x" []], ID "y" []])
                       (ID "s" [ID "z" [], ID "y" []]) 
                [CArrow (ID "add" [ID "x" [], ID "y" []]) 
                        (ID "z" [])])]},
   testCase "var {f(x), y} = {x, y}" $ 
    var [ID "f" [VarID "x" []], VarID "y" []]
    @?= ["x","y"]
  ],
  testGroup "Parser"
  [testCase "Parsing add-file CoCo format" $
    do act <- parseCCSFile "CCSs/add.coco.ccs"
       out <- fmap read (readFile "outputs/add.out") 
       act @?= Right out,
   testCase "Parsing add-file LOPSTR format" $
    do act <- parseCCSFile "CCSs/add.ccs"
       out <- fmap read (readFile "outputs/add.out") 
       act @?= Right out,
   testCase "Parsing addd-file" $
    do act <- parseCCSFile "src/addd.ccs"
       out <- fmap read (readFile "outputs/add.out") 
       act @?= Right out,
   testCase "Parsing example from CoCo website" $
    do act <- parseCCSFile "CCSs/ctrs-ex.ccs"
       out <- fmap read (readFile "outputs/ctrs-ex.out") 
       act @?= Right out,
   testCase "Parsing example 2 from CoCo website" $
    do act <- parseCCSFile "CCSs/ctrs-ex2.ccs"
       out <- fmap read (readFile "outputs/ctrs-ex2.out") 
       act @?= Right out,
   testCase "Parsing example 3 from CoCo website" $
    do act <- parseCCSFile "CCSs/ctrs-ex3.ccs"
       out <- fmap read (readFile "outputs/ctrs-ex3.out") 
       act @?= Right out,
   testCase "Parsing simple" $
    parseCCS "(VAR y a) (RULES f(a) -> <0>)" 
      @?= Right (Ccs {idlist = Var ["y", "a"], 
                      funlist = Sig [],
                      rulelist = Rules [Arrow (ID "f" [VarID "a" []]) 
                                              (ID "<>" [ID "0" []]) []]}) ],
  testGroup "Rule-inverters"
  [testCase "Trivial ruleinv : full, no conditions" $
       ruleinvTriv (Arrow (ID "f" [ID "a" []])
                          (ID "<>" [ID "b" []]) []) [] [] [0] 
               @?= Just (Arrow (ID "f{}{0}" [ID "b" []])
                               (ID "<>" [ID "a" []]) []),
   testCase "Trivial ruleinv : full, conditions" $
       ruleinvTriv (Arrow (ID "f" [ID "c" []])
                          (ID "<>" [ID "d" []]) 
                            [CArrow (ID "f" [ID "a" []])
                                    (ID "<>" [ID "b" []])]) [] [] [0] 
               @?= Just (Arrow (ID "f{}{0}" [ID "d" []])
                               (ID "<>" [ID "c" []]) 
                               [CArrow (ID "f" [ID "a" []])
                                       (ID "<>" [ID "b" []])]),
   testCase "Trivial ruleinv : error" $
       ruleinvTriv (Arrow (ID "f" [ID "c" []])
                          (ID "<>" [ID "d" []]) 
                            [CArrow (ID "f" [ID "a" []])
                                    (ID "<>" [ID "b" []])]) [] [] [2,5,1] 
               @?= Nothing,
   testCase "Trivial ruleinv : from file" $
    do rule <- fmap read (readFile "CCSs/addrule.in")
       res <- return (ruleinvTriv rule [] [] [0])
       exp <- fmap read (readFile "outputs/addrule-triv.out")
       res @?= Just exp,
   testCase "Pure full ruleinv : full, no conditions" $
       ruleinvPfull (Arrow (ID "f" [ID "a" []])
                          (ID "<>" [ID "b" []]) []) [] [] [0] 
               @?= Just (Arrow (ID "f{}{0}" [ID "b" []])
                               (ID "<>" [ID "a" []]) []),
   testCase "Pure full ruleinv : full, conditions" $
       ruleinvPfull (Arrow (ID "f" [ID "c" []])
                          (ID "<>" [ID "d" []]) 
                            [CArrow (ID "f" [ID "a" []])
                                    (ID "<>" [ID "b" []])]) [] [] [0] 
               @?= Just (Arrow (ID "f{}{0}" [ID "d" []])
                               (ID "<>" [ID "c" []]) 
                               [CArrow (ID "f{}{0}" [ID "b" []])
                                       (ID "<>" [ID "a" []])]),
   testCase "Pure full ruleinv : from file" $
    do rule <- fmap read (readFile "CCSs/addrule.in")
       res <- return (ruleinvPfull rule [] [] [0])
       exp <- fmap read (readFile "outputs/addrule-pfull.out")
       res @?= Just exp
  ],
  testGroup "localinv part/semi"
  [testCase "localinv-part 'f(x) -> y, f(y) -> z' {x, y}" $
     localinv [CArrow (ID "f" [VarID "x" []]) (ID "<>" [VarID "y" []]),
               CArrow (ID "f" [VarID "y" []]) (ID "<>" [VarID "z" []])]
              ["x", "y"] "part"
               @?= [CArrow (ID "f{0}{0}" [VarID "x" [], VarID "y" []])
                            (ID "<>" []),
                    CArrow (ID "f{0}{0}" [VarID "y" [], VarID "z" []])
                            (ID "<>" [])],
   testCase "localinv-semi 'f(x) -> y, f(y) -> z' {x, y}" $
     localinv [CArrow (ID "f" [VarID "x" []]) (ID "<>" [VarID "y" []]),
               CArrow (ID "f" [VarID "y" []]) (ID "<>" [VarID "z" []])]
              ["x", "y"] "semi"
               @?= [CArrow (ID "f{0}{0}" [VarID "x" [], VarID "y" []])
                            (ID "<>" []),
                    CArrow (ID "f{0}{}" [VarID "y" []])
                            (ID "<>" [VarID "z" []])]
  ],
  testGroup "heuristic"
  [testCase "'f(x) -> y, f(y) -> z' => f(y) -> z, f(x) -> y" $
     heuristic [CArrow (ID "f" [VarID "x" []]) (ID "<>" [VarID "y" []]),
                CArrow (ID "f" [VarID "y" []]) (ID "<>" [VarID "z" []])]
               ["z", "y"]
               @?= [CArrow (ID "f" [VarID "y" []]) (ID "<>" [VarID "z" []]),
                   CArrow (ID "f" [VarID "x" []]) (ID "<>" [VarID "y" []])],
   testCase "'f(x) -> y, f(y) -> z' => f(x) -> y, f(y) -> z" $
     heuristic [CArrow (ID "f" [VarID "x" []]) (ID "<>" [VarID "y" []]),
                CArrow (ID "f" [VarID "y" []]) (ID "<>" [VarID "z" []])]
               ["x", "y"]
               @?= [CArrow (ID "f" [VarID "x" []]) (ID "<>" [VarID "y" []]),
                   CArrow (ID "f" [VarID "y" []]) (ID "<>" [VarID "z" []])]
  ],
  testGroup "Properties"
  [testCase "EV-free true" $
    do ccs' <- parseCCSFile "CCSs/add.ccs"
       res <- case ccs' of
                Left err -> return $ False
                Right ccs -> return $ isEvFree ccs
       res @?= True,
   testCase "EV-free false" $
    do ccs' <- parseCCSFile "CCSs/not-evfree.ccs"
       res <- case ccs' of
                Left err -> return $ True
                Right ccs -> return $ isEvFree ccs
       res @?= False
  ,
  testCase "Left-to-right deterministic true" $
    do ccs' <- parseCCSFile "CCSs/add.ccs"
       res <- case ccs' of
                Left err -> return $ False
                Right ccs -> return $ isDeterm ccs
       res @?= True,
   testCase "Left-to-right deterministic false" $
    do ccs' <- parseCCSFile "CCSs/not-ltrdeterm.ccs"
       res <- case ccs' of
                Left err -> return $ True
                Right ccs -> return $ isDeterm ccs
       res @?= False,
  -- CHANGEEEEE
   testCase "Singular computational true" $
    do ccs' <- parseCCSFile "CCSs/add.ccs"
       res <- case ccs' of
                Left err -> return $ False
                Right ccs -> return $ isEcFree ccs
       res @?= True,
   testCase "Singular computational false" $
    do ccs' <- parseCCSFile "CCSs/romanenko-ack21.ccs"
       res <- case ccs' of
                Left err -> return $ True
                Right ccs -> return $ isEcFree ccs
       res @?= False,
   testCase "Non-erasing true" $
    do ccs' <- parseCCSFile "CCSs/romanenko-ackx.ccs"
       res <- case ccs' of
                Left err -> return $ False
                Right ccs -> return $ isNonerasing ccs
       res @?= True,
   testCase "Non-erasing false" $
    do ccs' <- parseCCSFile "CCSs/romanenko-ack21.ccs"
       res <- case ccs' of
                Left err -> return $ True
                Right ccs -> return $ isNonerasing ccs
       res @?= False,
   testCase "Weak non-erasing true" $
    do ccs' <- parseCCSFile "CCSs/romanenko-ack21.ccs"
       res <- case ccs' of
                Left err -> return $ False
                Right ccs -> return $ isWeaknonerasing ccs
       res @?= True,
   testCase "Weak non-erasing false" $
    do ccs' <- parseCCSFile "CCSs/weaknonerasing-false.ccs"
       res <- case ccs' of
                Left err -> return $ True
                Right ccs -> return $ isWeaknonerasing ccs
       res @?= False,
   testCase "Right-linear true" $
    do ccs' <- parseCCSFile "CCSs/ctrs-ex2.ccs"
       res <- case ccs' of
                Left err -> return $ False
                Right ccs -> return $ isRightlinearCCS ccs
       res @?= True,
   testCase "Right-linear false" $
    do ccs' <- parseCCSFile "CCSs/ctrs-ex3.ccs"
       res <- case ccs' of
                Left err -> return $ True
                Right ccs -> return $ isRightlinearCCS ccs
       res @?= False,
   testCase "Left-linear true" $
    do ccs' <- parseCCSFile "CCSs/ctrs-ex3.ccs"
       res <- case ccs' of
                Left err -> return $ False
                Right ccs -> return $ isLeftlinearCCS ccs
       res @?= True,
   testCase "Left-linear false" $
    do ccs' <- parseCCSFile "CCSs/ctrs-ex2.ccs"
       res <- case ccs' of
                Left err -> return $ True
                Right ccs -> return $ isLeftlinearCCS ccs
       res @?= False,
   testCase "Unifiable true" $
    do t1 <- return (ID "h" [ID "k" [VarID "A" []], VarID "B" []])
       t2 <- return (ID "h" [VarID "B" [], ID "k" [VarID "C" []]])
       res <- return $ overlap t1 t2
       res @?= True,
   testCase "Unifiable false" $
    do t1 <- return (ID "h" [ID "k" [VarID "A" []], VarID "B" []])
       t2 <- return (ID "h" [VarID "B" [], VarID "A" []])
       res <- return $ overlap t1 t2
       res @?= False,
   testCase "Unifiable false" $
    do t1 <- return (ID "<>" [ID "s" [VarID "x" []], ID "s" [VarID "z" []]])
       t2 <- return (ID "<>" [VarID "y" [], ID "0" []])
       res <- return $ overlap t1 t2
       res @?= False,
   testCase "Non-overlapping true" $
    do ccs' <- parseCCSFile "CCSs/mul.ccs"
       res <- case ccs' of
                Left err -> return $ False
                Right ccs -> return $ isNonOverlappingCCS ccs ccs
       res @?= True,
   testCase "Non-overlapping false" $
    do ccs' <- parseCCSFile "CCSs/ctrs-ex3.ccs"
       res <- case ccs' of
                Left err -> return $ True
                Right ccs -> return $ isNonOverlappingCCS ccs ccs
       res @?= False,
   testCase "Non-output-overlapping true" $
    do ccs' <- parseCCSFile "CCSs/ctrs-ex3.ccs"
       res <- case ccs' of
                Left err -> return $ False
                Right ccs -> return $ isNonOutputOverlappingCCS ccs ccs
       res @?= True,
   testCase "Non-output-overlapping false" $
    do ccs' <- parseCCSFile "CCSs/mul.ccs"
       res <- case ccs' of
                Left err -> return $ True
                Right ccs -> return $ isNonOutputOverlappingCCS ccs ccs
       res @?= False
  ],
  testGroup "Inversion (Testing everything together)"
  [testCase "Full inversion of mul using trivial ruleinv" $
    do ccs' <- parseCCSFile "CCSs/mul.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvTriv [InvTask "mul" [] [0]] ccs ccs [] [])
       exp <- fmap read (readFile "outputs/mul-triv-full.out")
       res @?= Right exp,
  testCase "Full inversion of add using trivial ruleinv" $
    do ccs' <- parseCCSFile "CCSs/add.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvTriv [InvTask "add" [] [0]] ccs ccs [] [])
       exp <- fmap read (readFile "outputs/add-triv-full.out")
       res @?= Right exp,
  testCase "Partial inversion of mul using trivial ruleinv" $
    do ccs' <- parseCCSFile "CCSs/mul.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvTriv [InvTask "mul" [0] [0]] ccs ccs [] [])
       exp <- fmap read (readFile "outputs/mul-triv-part.out")
       res @?= Right exp,
  testCase "Full inversion of perm using trivial ruleinv" $
    do ccs' <- parseCCSFile "CCSs/perm.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvTriv [InvTask "perm" [] [0]] ccs ccs [] [])
       exp <- fmap read (readFile "outputs/perm-triv-full.out")
       res @?= Right exp,
  testCase "Full inversion of mul using pfull ruleinv" $
    do ccs' <- parseCCSFile "CCSs/mul.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvPfull [InvTask "mul" [] [0]] ccs ccs [] [])
       exp <- fmap read (readFile "outputs/mul-full-full.out")
       res @?= Right exp,
  testCase "Full inversion of perm using pfull ruleinv" $
    do ccs' <- parseCCSFile "CCSs/perm.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvPfull [InvTask "perm" [] [0]] ccs ccs [] [])
       exp <- fmap read (readFile "outputs/perm-full-full.out")
       res @?= Right exp,
  testCase "Partial inversion of mul using pfull ruleinv" $
    do ccs' <- parseCCSFile "CCSs/mul.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvPfull [InvTask "mul" [0] [0]] ccs ccs [] [])
       exp <- fmap read (readFile "outputs/mul-full-part.out")
       res @?= Right exp,
  testCase "Full inversion of mul using part ruleinv" $
    do ccs' <- parseCCSFile "CCSs/mul.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvPart [InvTask "mul" [] [0]] ccs ccs [] [])
       exp <- parseCCSFile "CCSs/mul-part-full.ccs"
       res @?= exp,
  testCase "Full inversion of mul using semi ruleinv" $
    do ccs' <- parseCCSFile "CCSs/mul.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvSemi [InvTask "mul" [] [0]] ccs ccs [] [])
       exp <- parseCCSFile "CCSs/mul-part-full.ccs"
       res @?= exp,
  testCase "Full inversion of reversible add" $
    do ccs' <- parseCCSFile "CCSs/add-prime.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvPfull [InvTask "addprime" [] [0,1]] ccs ccs [] [])
       exp <- parseCCSFile "CCSs/add-prime-full.out.ccs"
       res @?= exp,
  testCase "Partial inversion of reversible add" $
    do ccs' <- parseCCSFile "CCSs/add-prime.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvPart [InvTask "addprime" [0] [0,1]] ccs ccs [] [])
       exp <- parseCCSFile "CCSs/add-prime-part.out.ccs"
       res @?= exp,
  testCase "Semi inversion of reversible add" $
    do ccs' <- parseCCSFile "CCSs/add-prime.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvSemi [InvTask "addprime" [] []] ccs ccs [] [])
       exp <- parseCCSFile "CCSs/add-prime-semi.out.ccs"
       res @?= exp,
  testCase "Full inversion of fall" $
    do ccs' <- parseCCSFile "CCSs/fall.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvSemi [InvTask "fall" [] [0,1]] ccs ccs [] [])
       exp <- parseCCSFile "CCSs/fall-semi-full.ccs"
       res @?= exp,
  testCase "Partial inversion of fall" $
    do ccs' <- parseCCSFile "CCSs/fall.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvSemi [InvTask "fall" [2] [0,1]] ccs ccs [] [])
       exp <- parseCCSFile "CCSs/fall-semi-part.ccs"
       res @?= exp,
  testCase "Partial inversion of fall" $
    do ccs' <- parseCCSFile "CCSs/fall.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvSemi [InvTask "fall" [0,2] [1]] ccs ccs [] [])
       exp <- parseCCSFile "CCSs/fall-semi-semi.ccs"
       res @?= exp,
  testCase "Encrypt to decrypt" $
    do ccs' <- parseCCSFile "CCSs/encrypt.ccs"
       res <- case ccs' of
                Left err -> return (Right (Ccs {idlist = Var [], funlist = Sig [], rulelist = Rules []}))
                Right ccs -> return (inv ruleinvSemi [InvTask "encrypt" [1] [0]] ccs ccs [] [])
       exp <- parseCCSFile "CCSs/decrypt.ccs"
       res @?= exp
  ]
  ]

main = defaultMain tests
