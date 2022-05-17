module Logic() where

import Prelude hiding (and, And)
import Text.Layout.Table
import Control.Monad
import Data.List
import Data.Maybe

-- Boolean Expression Data Type
-- Can be a:
-- 		SBool (Straight Boolean) - constant True or False
--		Var (Variable Boolean) - variable True or False
--		Buffer - Returns input
--		Not - Returns inverted input
--		And - Returns AND logic of list of BoolExp
--		Or - Returns OR logic of list of BoolExp
--		Nand - Returns inverted AND logic of list of BoolExp
--		Nor - Returns inverted OR logic of list of BoolExp
--		Xor - Returns exclusive OR logic of list of BoolExp
--		Xnor - Returns exclusive NOR logic of list of BoolExp
data BoolExp = SBool Bool
             | VBool String
             | Buffer BoolExp
             | Not BoolExp
             | And [BoolExp]
             | Or [BoolExp]
             | Nand [BoolExp]
             | Nor [BoolExp]
             | Xor [BoolExp]
             | Xnor [BoolExp]
             deriving (Eq)
  
instance Show BoolExp where
  show (SBool True) = show True
  show (SBool False) = show False
  show (VBool x) = x
  show (Buffer x) = "Buffer -> (" ++ show x ++ ")"
  show (Not x) = "~" ++ (show x)
  show (And x) = "(" ++ toString x ++ ")"
    where toString (a:b) = if (b == []) then show a 
                           else show a ++ " AND " ++ toString b
  show (Or x) = "(" ++ toString x ++ ")"
    where toString (a:b) = if (b == []) then show a 
                           else show a ++ " OR " ++ toString b
  show (Nand x) = "~" ++ show (And x)
  show (Nor x) = "~" ++ show (Or x)
  show (Xor x) = "(" ++ toString x ++ ")"
    where toString (a:b) = if (b == []) then show a 
                           else show a ++ " XOR " ++ toString b
  show (Xnor x) = "~" ++ show (Xor x)
  
-- Function eval
-- Takes a list of variables and their values and a Boolean expression, then outputs evaluated expression as SBool.
-- List of variables taken as list of tuples (VBool name, Assigned Value)
eval :: [(String, Bool)] -> BoolExp -> Bool
eval vars exp = case exp of
  SBool x -> x
  VBool x -> getBoolVal vars x
    where getBoolVal [] _ = False --NEED TO PUT AN ERROR CASE IN HERE
          getBoolVal ((varName, boolVal):rest) name = if (name == varName) then boolVal
                                                      else getBoolVal rest name
  Buffer x -> eval vars x
  Not x -> not (eval vars x)
  And x -> foldr (&&) (True) (map (\y -> eval vars y) x)
  Or x -> foldr (||) (False) (map (\y -> eval vars y) x)
  Nand x -> not (eval vars (And x))
  Nor x -> not (eval vars (Or x))
  --xor
  --xnor
  
-- Truth Tables
data TruthTable = TruthTable (BoolExp, [[Bool]])

instance Show TruthTable where
  show (TruthTable (exp, truthValues)) = tableString (replicate (length truthValues) (column expand left def def))
                                                      unicodeS
                                                      (titlesH ((getVarNames exp) ++ [show exp]))
                                                      (map (rowG) (map (map (boolToStr)) (truthValues)))
                                         where boolToStr b = if (b == True) then "1" else "0"

-- Function getVarNames
-- Take a BoolExp and returns all VBool variables names as a list of strings in alphabetical order.
getVarNames :: BoolExp -> [String]
getVarNames exp = case exp of
  SBool x -> []
  VBool x -> [x]
  Buffer x -> getVarNames x
  Not x -> getVarNames x
  And x -> sort $ nub $ foldr (++) ([]) (map getVarNames x)
  Or x -> sort $ nub $ foldr (++) ([]) (map getVarNames x)
  Nand x -> sort $ nub $ foldr (++) ([]) (map getVarNames x)
  Nor x -> sort $ nub $ foldr (++) ([]) (map getVarNames x)
  --xor
  --xnor

getTruthCombos :: Int -> [[Bool]]
getTruthCombos n = sequence (replicate n [True, False])

-- Function truthTable
-- Takes a Boolean expression and returns the truth table for that boolean expression. 
truthTable :: BoolExp -> TruthTable
truthTable exp = 
  let vars = getVarNames exp
      numVars = length vars
      truthValues = reverse (getTruthCombos numVars)
      varVals = map (zip vars) truthValues
      results = (map (\x -> eval x exp) varVals)
      table = map (\(x,y) -> x ++ [y]) (zip truthValues results)
  in TruthTable (exp, table)

-- Quine-McCluskey Algorithm
getMintermsExp :: BoolExp -> BoolExp
getMintermsExp exp = 
  let vars = getVarNames exp
      numVars = length vars
      truthValues = reverse (getTruthCombos numVars)
      varVals = map (zip vars) truthValues
      results = filter (\x -> snd x == True) (zip varVals (map (\x -> eval x exp) varVals))
      minterms = (map (makeSOP) (map (fst) results))
  in Or (map (\x -> And x) minterms)
    where makeSOP [] = []
          makeSOP ((var, val):y) = if (val == True) then (VBool var) : makeSOP y
                                   else (Not (VBool var)) : makeSOP y

getMinterms :: Int -> [[Int]]
getMinterms n = sequence (replicate n [1, 0])

--Special thanks to @MartinFinke for providing some backbone to the Quine-McCluskey Algorithm in Haskell
--Original source code was modified to meet the data types established for Haskell-gebra.
--Used methods (safeHead, smallest, merge, reduce, primeImplicant, minimalize)
--https://github.com/martinfinke/qm
data Bit = F | T | X deriving (Show, Enum, Ord, Eq)

type BitSet = [Bit]

type Minterm = ([Integer], BitSet)

type Table = [Minterm]

safeHead::[a] -> Maybe a
safeHead [] = Nothing
safeHead (t:_) = Just t

smallest::[[a]] -> [a]
smallest [] = error "empty list"
smallest a = reverse.fst $ smallest' $ map ((,) []) a
   where 
       smallest' a = case find cmp a of
                          Just l -> l
                          Nothing -> smallest' $ map helper a
                          
       cmp (_ , []) = True
       cmp _ = False
       helper (as, (b:bs)) = (b:as, bs)
   
merge::Minterm -> Minterm -> Maybe Minterm
merge (m1, b1) (m2, b2) = if isJust $ merge' b1 b2
                             then Just (m1++m2, fromJust $ merge' b1 b2)
                             else Nothing
   where 
       merge'::BitSet -> BitSet -> Maybe BitSet
       merge' [] [] = Nothing
       merge' (m:ms) (n:ns)
           | m == n             = fmap (m:) $ merge' ms ns
           | m /= n && ms == ns = fmap (X:) $ Just ms
           | otherwise          = Nothing
           
reduce:: Table -> Table
reduce [] = []
reduce t = clear $ (reduce $ reduce' t) ++ t
   where 
       reduce' [] = []
       reduce' (m:ts) = mapMaybe (merge m) ts ++ reduce' ts
       clear [] = []
       clear (t:ts) = t:(clear $ filter (t `notIn`) ts)
       notIn (t, _) (a, _) = not.and $ map (`elem` t) a

primeImplicant::Table -> Maybe Integer
primeImplicant = fmap head . safeHead . filter ((1==).length) . group . sort . concat. map fst
          
minimalize:: Table -> [BitSet]
minimalize [] = []
minimalize t = case primeImplicant t of
                    Just p -> (snd $ term p t):(minimalize $ removeMinterms (fst $ term p t) t)
                    Nothing -> smallest $ map minimalize' $ zipWith (++) (inits t) (tail $ tails t)
   where
       term::Integer -> Table -> Minterm
       term m = head . filter (\(x, _) -> m `elem` x)
       removeMinterms ms = filter (not.null.fst) .map (\(x, b) -> (filter (not.(`elem` ms)) x , b))
       minimalize' t = (snd $ head t):(minimalize $ removeMinterms (fst $ head t) t)

getPrimeImplicants :: BoolExp -> Table
getPrimeImplicants exp =
  let vars = getVarNames exp
      numVars = length vars
      truthValues = reverse (getTruthCombos numVars)
      varVals = map (\x -> ([fst x],(snd x))) (zip [0..] (map (zip vars) truthValues))
      results = filter (\x -> snd x == True) (zip varVals (map (\x -> eval (snd x) exp) varVals))
      mintermId = map fst (map (fst) results)
      mintermVals = map (makeSOP) (map (snd) (map (fst) results))
      minterms = zip mintermId mintermVals
  in minterms
    where makeSOP [] = []
          makeSOP ((var, val):y) = if (val == True) then T : makeSOP y
                                   else F : makeSOP y

simplify::BoolExp -> BoolExp
simplify exp = 
  let bitVals = minimalize $ reduce $ getPrimeImplicants exp
      vars = getVarNames exp
  in Or (reverse $ convert $ map (\x -> zip vars x) bitVals)
    where convert lst = map (\x -> And (evalBits x)) lst

evalBits::[(String, Bit)] -> [BoolExp]
evalBits [] = []
evalBits ((var, bit):rest) = if (bit == T) then (VBool var) : evalBits rest
                             else if (bit == F) then (Not (VBool var)) : evalBits rest
                             else evalBits rest