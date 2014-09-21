module Sol1 where

import Week3

topF :: Form
topF = Cnj []

bottomF :: Form
bottomF = Dsj []

--Time: 15 minutes
contradiction :: Form -> Bool
contradiction = not.satisfiable

-- Time: 15 minutes
tautology :: Form -> Bool
tautology = not.falsifiable

-- same as satisfiable, but instead gives true if one of the values evaluates to false.
falsifiable :: Form -> Bool
falsifiable f = any (\ v -> not $ eval v f) (allVals f)

-- checked using known contradictions and tautologies (From google and book)

-- logical entailment time: 20 minutes
entails :: Form -> Form -> Bool
entails f1 f2 = tautology $ Impl f1 f2                                        
            
-- logical equivalence time: 2 minutes
equiv :: Form -> Form -> Bool
equiv f1 f2  = tautology $ Equiv f1 f2                     
            
-- Exercise 2: total: 9:30 hours
toNNF :: Form -> Form
toNNF = nnf.arrowfree

toCNF = flattenForm.cnf.toNNF

-- Pre: parameter is in NNF and does not contain any operators except for and, or and negation
-- Post: the result is in CNF
cnf :: Form -> Form
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj []) = Dsj []
cnf (Dsj fs) = foldDist $ Dsj (map cnf fs)
cnf f        = f

-- fold the distribution list
-- Pre: the parameter is a non-empty disjunction and each disjunct is in CNF
-- Post: result is the parameter in CNF
foldDist :: Form -> Form
foldDist (Dsj xs) = foldr1 dist xs
foldDist _ = error "Precondition failed: Argument is not a distribution"

-- distribute two Forms over each other
-- Pre: the parameters are in CNF
-- Post: the two parameters folded together in an equivalent CNF formula
dist :: Form -> Form -> Form
dist p (Cnj [])        = topF                  -- Cnj [] is a tautology, Dsj[... Cnj[] ...] = tautology, so don't merge, keep the Cnj []    
dist (Cnj []) p        = topF                  -- see above (if you want to keep all variables, just use Cnj[Dsj[p, cnf neg p]]. This makes your formulas a lot longer though.
dist p (Cnj xs)        = Cnj $ map (dist p) xs -- distribute p over the list xs in the conjunction        
dist (Cnj xs) p        = Cnj $ map (dist p) xs
dist p p2              = Dsj (p:[p2])          -- Merge disj

flattenForm :: Form -> Form 
flattenForm (Cnj xs) = Cnj $ flatCnj xs
flattenForm (Dsj xs) = Dsj $ flatDsj xs 
flattenForm p        = p

--Flattens the list of a Cnj recursively
flatCnj :: [Form] -> [Form]
flatCnj []            = []
flatCnj ((Cnj ys):xs) = flatCnj ys ++ flatCnj xs
flatCnj (p:xs)        = (flattenForm p) : flatCnj xs

--Flattens the list of a Dsj recursively
flatDsj :: [Form] -> [Form]
flatDsj []            = []
flatDsj ((Dsj ys):xs) = flatDsj ys ++ flatDsj xs
flatDsj (p:xs)        = (flattenForm p) : flatDsj xs 



---------------------------------------------------------------- Test cases

--start 1 : 17:09 - stop1: 18:30, start2 12:30 stop2: 14:30, start3 15:15, stop3: 16:40                
-- Extra: 6 hours
runCNFTests = do
              originals1 <- getRndFs 2 30
              let cnfs = map toCNF originals1
              mapM_ putStrLn $ map show cnfs
              putStrLn ""
              putStrLn ""
              putStrLn "Check if flat CNF:" 
              putStrLn $ show $ map isFlatCNF cnfs
              
-- Flat variant (nested CNJ and DSJ are not allowed)
isFlatCNF :: Form -> Bool
isFlatCNF (Cnj xs)       = and $ map allowedCnj xs   
isFlatCNF (Dsj xs)       = and $ map allowedDsj xs
isFlatCNF (Prop p)       = True
isFlatCNF (Neg (Prop p)) = True
isFlatCNF _              = False

-- children of Cnj may only be Dsj, Prop p or Neg (Prop p)
allowedCnj :: Form -> Bool
allowedCnj (Dsj x)        = and $ map allowedDsj x
allowedCnj (Prop p)       = True
allowedCnj (Neg (Prop p)) = True
allowedCnj  _             = False

-- children of Dsj may only be Prop p or Neg (Prop p)
allowedDsj :: Form -> Bool
allowedDsj (Prop p)       = True
allowedDsj (Neg (Prop p)) = True
allowedDsj _              = False

-- check if the functions returned by toCNF are equivalent to the original function

checkPropFalse :: [Form] -> (Form -> Bool) -> Maybe Form
checkPropFalse xs f = foldr (\a b -> if not $ f a then Just a else b) Nothing xs

--checkTestcasesCNF :: Form -> String -> IO ()
--checkTestcasesCNF xs name = do
--                            let forms = isCNF
                                        
runTests = do
            originals1 <- getRndFs 1 100 -- Small testcases
            originals2 <- getRndFs 2 50  -- Medium testcases
            originals3 <- getRndFs 3 10  -- Large testcases
            
            let cnfs = map toCNF originals1                     -- convert to cnf
            let isCNF = and $ map isFlatCNF cnfs                -- check if isFlatCNF
            let equiv1 = zipWith (equiv) originals1 cnfs  -- 
            putStrLn ""
            putStrLn ""
            putStrLn ""
            putStrLn "Originals2:"
            mapM_ putStrLn $ map show originals2
            let cnfs = map toCNF originals2
            let result2 = zipWith (equiv) originals2 cnfs
            putStrLn ""
            putStrLn ""
            putStrLn ""
            putStrLn "Originals3:"
            mapM_ putStrLn $ map show originals3
            let cnfs = map toCNF originals3
            let result3 = zipWith (equiv) originals3 cnfs
            putStrLn ""
            putStrLn ""
            putStrLn $ "Results1:" ++ (show (and equiv1))
            putStrLn $ drop 1 $ show equiv1
            putStrLn ""
            putStrLn ""
            putStrLn $ "Results2:" ++ show (and result2)
            putStrLn $ drop 1 $ show result2
            putStrLn ""
            putStrLn ""
            putStrLn $ "Results3:" ++ show (and result3)
            putStrLn $ drop 1 $ show result3 
            
            
-------------------------------------------------------------------------------------------------------------- Exercise 4

--start1 12:00 stop1: 15:30 

type Clause = [Int]
type Clauses = [Clause]

topC :: Clauses
topC = []       -- Empty conjunction, thus Empty Clauselist

bottomC :: Clauses
bottomC = [[]]  -- Empty Disjunction in Conjunction, thus Clauselist with empty clause

-- Turn a cnf Form into clauses
cnf2clss :: Form -> Clauses
cnf2clss (Cnj xs)       = cnj2clss xs           
cnf2clss (Dsj xs)       = [dsj2cls xs]
cnf2clss (Neg (Prop n)) = [[-n]]
cnf2clss (Prop n)       = [[n]]

cnj2clss :: [Form] -> Clauses
cnj2clss []            = []
cnj2clss ((Dsj xs):ys) = dsj2cls xs : (cnj2clss ys)
cnj2clss (p:xs)        = [toInt p]: (cnj2clss xs)

dsj2cls :: [Form] -> Clause
dsj2cls = foldr (\a b -> (toInt a) : b) []

-- Turn clauses into a cnf Form
clss2cnf :: Clauses -> Form
clss2cnf xs = Cnj $ map cls2dsj xs

cls2dsj :: Clause -> Form
cls2dsj xs = Dsj $ foldr (\a b -> (fromInt a) : b) [] xs

-- conversion utility functions
toInt :: Form -> Int
toInt (Prop n)       = n
toInt (Neg (Prop n)) = -n

fromInt :: Int -> Form
fromInt n | n < 0 = Neg $ Prop $ -n
          | otherwise = Prop n
          
-- Clause tests --------------------------------------------------------------------------------------------------------

runClauseTests = do
                  originals1 <- getRndFs 1 500  -- Small testcases
                  originals2 <- getRndFs 2 50  -- Medium testcases
                  originals3 <- getRndFs 3 10  -- Large testcases
                  
                  putStrLn "Run 1..."
                  let cnfs = map toCNF originals1  -- convert to cnf
                  let clss = map cnf2clss cnfs     -- convert to clauses
                  let cnfs2 = map clss2cnf clss
                  let equiv1 = and $ zipWith (equiv) cnfs cnfs2
                  putStrLn $ "Run 1 result:" ++ (show equiv1)
                  if not equiv1 then do
                                        putStrLn "Clss:"
                                        mapM_ putStrLn $ map show clss
                                        putStrLn ""
                                        putStrLn "Cnf1:"
                                        mapM_ putStrLn $ map show cnfs
                                        putStrLn ""
                                        putStrLn ""
                                        putStrLn "Cnf2:"
                                        mapM_ putStrLn $ map show cnfs2
                  else return ()
                  putStrLn "Run 2..."
                  let cnfs = map toCNF originals2
                  let clss = map cnf2clss cnfs
                  let cnfs2 = map clss2cnf clss
                  let equiv2 = and $ zipWith (equiv) cnfs cnfs2
                  putStrLn $ "Run 2 result:" ++ (show equiv2)
                  if not equiv2 then do
                                        putStrLn "Clss:"
                                        mapM_ putStrLn $ map show clss
                                        putStrLn ""
                                        putStrLn "Cnf1:"
                                        mapM_ putStrLn $ map show cnfs
                                        putStrLn ""
                                        putStrLn ""
                                        putStrLn "Cnf2:"
                                        mapM_ putStrLn $ map show cnfs2
                  else return ()
                  putStrLn ""
                  putStrLn "Run 3... (this one might take a while)"
                  let cnfs = map toCNF originals3
                  let clss = map cnf2clss cnfs
                  let cnfs2 = map clss2cnf clss
                  let equiv3 =  and $ zipWith (equiv) cnfs cnfs2 
                  putStrLn $ "Run 3 result:" ++ (show equiv3)
                  if not equiv2 then do
                                        putStrLn "Clss:"
                                        mapM_ putStrLn $ map show clss
                                        putStrLn ""
                                        putStrLn "Cnf1:"
                                        mapM_ putStrLn $ map show cnfs
                                        putStrLn ""
                                        putStrLn ""
                                        putStrLn "Cnf2:"
                                        mapM_ putStrLn $ map show cnfs2
                  else return ()
                  
                    
p1 = Prop 3
t1 = equiv (p1) (Cnj[Dsj[p1]])
