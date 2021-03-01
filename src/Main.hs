module Main where

import System.Environment
--import System.Directory
import System.IO

import CCSAst
import CCSParser
import CCSPretty
import Inverter
import Tex

strToList :: String -> [Int]
strToList str = map (\x -> x-1) (map read $ words str)

main :: IO ()
main = do (command:rest) <- getArgs
          case command of
            "inv" -> do
              -- path: path to file
              -- f: name of function you want to invert
              -- istr: io set in format "1 2 3"
              -- ostr: io set in format "1 2 3"
              let (path:f:istr:ostr:[]) = rest
              let file1 = "results/" ++ f ++ "triv.out"
              let file2 = "results/" ++ f ++ "full.out"
              let file3 = "results/" ++ f ++ "part.out"
              let file4 = "results/" ++ f ++ "semi.out"
              let i = strToList istr
              let o = strToList ostr
              parsed <- parseCCSFile path
              case parsed of
                Left err -> putStr err
                Right ccs ->
                  do let triv = (inv ruleinvTriv [InvTask f i o] ccs ccs [] [])
                         full = (inv ruleinvPfull [InvTask f i o]
                                 ccs ccs [] [])
                         part = (inv ruleinvPart [InvTask f i o]
                                 ccs ccs [] [])
                         semi = (inv ruleinvSemi [InvTask f i o]
                                 ccs ccs [] [])
                     case triv of
                          Left err -> putStr ("Applying the trivial rule inverter failed with message:\n " ++ err ++ "\n")
                          Right tccs -> let content = makepretty tccs
                                        in writeFile file1 content
                     case full of
                       Left err -> putStr ("Applying the pure full rule inverter failed with message:\n " ++ err ++ "\n")
                       Right fccs -> let content = makepretty fccs
                                     in writeFile file2 content
                     case part of
                       Left err -> putStr ("Applying the partial rule inverter failed with message:\n " ++ err ++ "\n")
                       Right pccs -> let content = makepretty pccs
                                     in writeFile file3 content
                     case semi of
                       Left err -> putStr ("Applying the semi rule inverter failed with message:\n " ++ err ++ "\n")
                       Right sccs -> let content = makepretty sccs
                                     in writeFile file4 content
            "diagnostics" -> do
              -- diagType: full or comparison?
              -- path: file to path
              let (diagType:path:rest') = rest
              parsed <- parseCCSFile path
              case diagType of
                "full" -> do
                  case parsed of
                    Left err -> putStr err
                    Right ccs -> putStr $ diagpretty path ccs
                "compare" -> do
                  let (pathOrig:[]) = rest'
                  parsedOrig <- parseCCSFile pathOrig
                  case parsed of
                    Left err -> putStr err
                    Right ccs -> 
                      case parsedOrig of
                        Left err -> putStr err
                        Right ccsOrig -> 
                          putStr $ comppretty path pathOrig ccs ccsOrig
            "latex" -> do
              let (path:name:flat:[]) = rest
              -- path: path to file
              -- name: name you want for file
              -- flat: write "flat" if you want a flat format
              makelatexFromFile path ("tex/" ++ name) flat
--(makepretty ccs) >>= \res -> putStrLn (show res)
