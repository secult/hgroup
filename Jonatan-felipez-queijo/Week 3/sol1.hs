module Sol1 where

import Week3

--Time: 15 minutes
contradiction :: Form -> Bool
contradiction = not.satisfiable

-- Time: 15 minutes
tautology :: Form -> Bool
tautology = not.falsifiable

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

toCNF = cnf.toNNF

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
dist p (Cnj []) = Cnj [] -- Cnj [] is a tautology, Dsj[... Cnj[] ...] = tautology, so don't merge, keep the Cnj []    
dist (Cnj []) p = Cnj [] -- see above
dist p (Cnj xs) = Cnj (map (dist p) xs) -- distribute p over the list xs in the conjunction        
dist (Cnj xs) p = Cnj (map (dist p) xs) -- see above
dist p p2       = Dsj (p:[p2])          -- Merge disj

------------------------------------------------------------------ Simplify: bugged

tosimpleCNF = simpl.toCNF

simpl :: Form -> Form
simpl (Cnj fs) = flatten $ Cnj fs
simpl (Dsj fs) = flatten $ Dsj fs
simpl f        = f

-- flatten a list conjunctions and disjunctions recursively
flatten :: Form -> Form
flatten (Cnj x) = Cnj $ cnjList ++ dsjList ++ rest
               where 
                  cnjList = concat (map (getFormLst.flat) (filter isCnj x))  -- aggregate all Cnj's
                  dsjList = map flat (filter isDsj x)                        -- flatten all Dsjs 
                  rest = filter (\y -> (not $ isCnj y) && (not $ isDsj y)) x -- don't simplify the rest

flatten (Dsj x) = Dsj $ dsjList ++ cnjList ++ rest
               where 
                  dsjList = concat (map (getFormLst.flat) (filter isDsj x)) -- aggregate all Cnj's
                  cnjList = map flat (filter isCnj x)                       -- flatten all Dsjs 
                  rest = filter (\y -> (not $ isCnj y) && (not $ isDsj y)) x -- leave the rest as it is    
flatten f = f

-- build up a flattened conjunction out of a conjunction or a disjunction out of a disjunction
flat :: Form -> Form
flat (Cnj xs) = foldr (\a (Cnj b) -> Cnj $ if isCnj a 
                                           then getFormLst (flatten a) ++ b 
                                           else (flatten a) : b) (Cnj []) xs 

flat (Dsj xs) = foldr (\a (Dsj b) -> Dsj $ if isDsj a 
                                           then getFormLst (flatten a) ++ b 
                                           else (flatten a) : b) (Dsj []) xs                                   
                                           
-- Get the list of a conjunction or a disjunction
getFormLst :: Form -> [Form]
getFormLst (Cnj xs) = xs
getFormLst (Dsj xs) = xs
getFormLst p = error ("Only works on listed types: " ++ show p)

isCnj :: Form -> Bool
isCnj (Cnj _) = True
isCnj _       = False

isDsj :: Form -> Bool
isDsj (Dsj _) = True
isDsj _       = False

---------------------------------------------------------------- Test cases

--start 1 : 17:09 - stop1: 18:30, start2 12:30 stop2: 14:30, start3 15:15, stop3: 16:40                

-- check if the functions returned by toCNF are indeed in CNF
runToCNFTests = do 
                originals1 <- getRndFs 2 10
                mapM_ putStrLn $ map show originals1
                let cnfs = map toCNF originals1
                putStrLn ""
                putStrLn ""
                putStrLn ""
                mapM_ (putStrLn.show ) cnfs

-- check if the functions returned by toCNF are equivalent to the original function                 
runTests = do
            originals1 <- getRndFs 1 100
            putStrLn "Originals1:"
            mapM_ putStrLn $ map show originals1
            let cnfs = map toCNF originals1
            --putStrLn ""
            --putStrLn "Originals in CNF:"
            --mapM_ putStrLn $ map (\a -> show a ++ ['\n']) cnfs
            putStrLn "testing equivalence with originals, this may take a while..."
            let result1 = zipWith (equiv) originals1 cnfs
            
            originals2 <- getRndFs 2 50
            putStrLn "Originals2:"
            mapM_ putStrLn $ map show originals2
            let cnfs = map toCNF originals2
            --putStrLn ""
            --putStrLn "Originals in CNF:"
            --mapM_ putStrLn $ map (\a -> show a ++ ['\n']) cnfs
            putStrLn "testing equivalence with originals, this may take a while..."
            let result2 = zipWith (equiv) originals2 cnfs
            
            originals3 <- getRndFs 3 10
            putStrLn "Originals3:"
            mapM_ putStrLn $ map show originals3
            let cnfs = map toCNF originals3
            --putStrLn ""
            --putStrLn "Originals3 in CNF:"
            --mapM_ putStrLn $ map (\a -> show a ++ ['\n']) cnfs
            putStrLn "testing equivalence with originals, this may take a while..."
            let result3 = zipWith (equiv) originals3 cnfs
            return (result1,result2,result3) 
            
            
            
            
            
            
            
