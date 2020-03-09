module Main        (main) where
import Prelude     ((.),($),lines,unlines,show,String,foldr,(++),fmap,(=<<),(>>),(>>=),head,length,max,putStrLn)
import Hproc.Hproc (hproc)
import System.IO   (IO,putStr,readFile)
import Text.Printf (printf)
import System.Environment (getArgs)
import System.Exit (exitWith,ExitCode( ExitSuccess ))
procStatFile="/proc/stat"
getHead :: [String] -> String
getHead [] = ""
getHead x  = head x
maxl l = foldr (\x a -> max (length x) a) 0 (fmap (\(c,_) -> c) l) 
showline c = printf ("%-"++(Prelude.show c)++"s : %% %s")
show l     = foldr (\(n,v) a -> (Main.showline (maxl l) n v):a) [] l
mainhproc = putStr . unlines . Main.show . hproc . getHead . lines =<< readFile procStatFile
main :: IO ()
main = do 
  getArgs >>= parse
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = mainhproc >> exit

usage   = putStr $ unlines $ ["Usage: hproc is using /proc/stat to compute the percentage of cpu per usage.",
                              "       just run : hproc"]
version = putStrLn "1.0.0.0"
exit    = exitWith ExitSuccess
