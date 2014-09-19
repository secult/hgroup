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
entails f1 f2 | propNames f1 /= propNames f2 = error "Incomparable formulas"
              | otherwise                    = tautology $ Impl f1 f2                           
     
-- logical equivalence time: 2 minutes
equiv :: Form -> Form -> Bool
equiv f1 f2 | propNames f1 /= propNames f2  = error "Incomparable formulas"
            | null $ propNames f1           = True -- If both formulas have no variables, then they are equivalent as well
            | otherwise                     = tautology $ Equiv f1 f2

-- Exercise 3: total: 9:30 hours

toCNF :: Form -> Form
toCNF = simpl.cnf.nnf.arrowfree

simpl :: Form -> Form
simpl (Cnj fs) = flatten $ Cnj fs
simpl (Dsj fs) = Dsj fs
simpl f        = f

-- Pre: parameter is in NNF and does not contain any operators except for and, or and negation
-- Post: the result is in CNF
cnf :: Form -> Form
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj []) = Cnj []
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
-- Post: the two parameters folded together in CNF
dist :: Form -> Form -> Form
dist p (Cnj []) = p 
dist (Cnj []) p = p 
dist p (Cnj xs) = Cnj (map (dist p) xs)
dist (Cnj xs) p = Cnj (map (dist p) xs)
dist p p2       = Dsj (p:[p2])       

-- flatten a list conjunctions and disjunctions recursively
-- Pre: Form is arrowfree and in NNF
-- Post: A flattened Form where nested conjunctions and disjunctions have been combined
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
-- Pre: Form has to be a conjunction or a disjunction
-- Post: Result is either a recursively flattened conunction or disjunction
flat :: Form -> Form
flat (Cnj xs) = foldr (\a (Cnj b) -> Cnj $ if isCnj a 
                                           then getFormLst (flatten a) ++ b 
                                           else (flatten a) : b) (Cnj []) xs 

flat (Dsj xs) = foldr (\a (Dsj b) -> Dsj $ if isDsj a 
                                           then getFormLst (flatten a) ++ b 
                                           else (flatten a) : b) (Dsj []) xs

-- Get the list of a conjunction or a disjunction
-- Pre: argument has to be a conjunction or a disjunction                                           
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

-- Failure on:
-- (-+(-8 +(5 8 13 11 16) -11 -12 4)<=>(18<=>+(*() (9<=>18) (10<=>16))))
-- (*(+((15<=>2)) (5<=>+(6 9 8 21 10)))==>(*()<=>+((19==>7) 9 11)))

--start 1 : 17:09

testcases = [   Dsj [], 
                Impl (Prop 1) (Prop 2), 
                Neg (Neg $ Prop 3), 
                Dsj [Cnj []], 
                Dsj [Dsj[Prop 2]],
                Cnj [Dsj[Prop 19, Prop 2, Prop 8, Prop 14, Prop 6], Equiv (Prop 14) (Prop 13), Dsj[Prop 5,Prop 5,Prop 2], Dsj[Prop 20], Dsj[Neg $ Prop 10]]
                ]
        
testresults = zipWith (equiv) originals cnfs
            where
                originals = testcases
                cnfs = map toCNF testcases 


runTests = do
            originals <- getRndFs 4 5
            putStrLn "Originals:"
            mapM_ putStrLn $ map show originals
            let cnfs = map toCNF originals
            putStrLn ""
            putStrLn "Originals in CNF:"
            mapM_ putStrLn $ map (\a -> show a ++ ['\n']) cnfs
            putStrLn "testing equivalence with originals, this may take a while..."
            let result = zipWith (equiv) originals cnfs
            return result --mapM_ (putStrLn.show) testfs
                
t2 = Cnj (map Prop [2..10])    

testRndFs :: IO [Form]
testRndFs = do 
                x <- getRndFs 2 4
                return x  
            
            
            
            
            
            
19
(--(15<=>1)<=>-*(-15 -8 (15<=>21) +(3 8 16)))
-+(((21==>14)<=>*(10)) 6)

(-
        +(
            -8 
            +(5 8 13 11 16) 
            -11 
            -12 
             4
         )
    <=>
        (
            18
          <=>
            +(
              *() 
              (
                 9 
               <=>
                 18
              ) 
              (
                  10
                <=>
                  16
              )
            )
        )

)

Neg Dsj [Neg (Prop 8), Cnj $ map Prop [5, 8, 13, 11, 16], Neg



(*(+((15<=>2)) (5<=>+(6 9 8 21 10)))==>(*()<=>+((19==>7) 9 11)))            
            
            
            
            
            
            
            
