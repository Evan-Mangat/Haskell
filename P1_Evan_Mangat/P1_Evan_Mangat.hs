import qualified Data.Map.Strict as Map
import Data.List (nub)
import System.IO ()
import System.Environment ()   

type VarId = String
data Prop = Const Bool
    | Var VarId
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Iff Prop Prop
    deriving (Eq, Read, Show)
type VarAsgn = Map.Map VarId Bool

--
findVarIds :: Prop -> [VarId]
findVarIds (Const e) = []
findVarIds (Var e1) = [e1]
findVarIds (Not e) = findVarIds e
findVarIds (And e1 e2) = nub(findVarIds e1 ++ findVarIds e2)
findVarIds (Or e1 e2) = nub(findVarIds e1 ++ findVarIds e2)
findVarIds (Imply e1 e2) = nub(findVarIds e1 ++ findVarIds e2)
findVarIds (Iff e1 e2) = nub(findVarIds e1 ++ findVarIds e2)

--
genVarAsgns :: [VarId] -> [VarAsgn]
genVarAsgns [] = [Map.empty]
genVarAsgns (x:xs) = map (Map.insert x True) ts ++ map (Map.insert x False) ts where ts = genVarAsgns xs

--     
eval :: Prop -> VarAsgn -> Bool
eval (Const c) _ = c
eval (Var v) bs      =  
    case Map.lookup v bs of
        Nothing -> False
        Just a -> a    
eval (Not e) bs      = not (eval e bs)
eval (And e1 e2) bs = eval e1 bs && eval e2 bs
eval (Or e1 e2) bs = eval e1 bs || eval e2 bs
eval (Imply e1 e2) bs = not (eval e1 bs) || eval e2 bs
eval (Iff e1 e2) bs = eval e1 bs && eval e2 bs
--
sat :: Prop -> Bool
sat p = elem True (map (eval p) (genVarAsgns(findVarIds p)))
--
readFormula :: String -> Prop
readFormula s = read s :: Prop 
--
checkFormula :: String -> String
checkFormula s = if sat (readFormula s) then "SAT" else "UNSAT"


main :: IO [()]
main = do
    contents <- getContents
    let ls = lines contents
    mapM putStrLn (map checkFormula ls)

    



                    
                    

    




