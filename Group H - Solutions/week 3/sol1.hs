module Sol1 where

import Week3

{-------------------------------------------------------------
    
    Exercise 1    
    Time spent: 52 min
--------------------------------------------------------------}

-- If a conjunction is defined as a formula which is False only if at least one of its components is False, 
-- then an empty conjunction is a tautology 
topF :: Form
topF = Cnj []

-- If a disjunction is defined as a formula which is True only if at least one of its components is True,
-- then an empty disjunction is a contradiction
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

-- logical entailment time: 20 minutes
entails :: Form -> Form -> Bool
entails f1 f2 = tautology $ Impl f1 f2                                        
            
-- logical equivalence time: 2 minutes
equiv :: Form -> Form -> Bool
equiv f1 f2  = tautology $ Equiv f1 f2                     


{----------------------------------------------------------------------------------------------------------------------
    
    Method of checking correctness:
    
    - looked at the function to see if it seemed correct 
    - fed examples from the workshop into the function to see if it produces the expected result (e.g. Demorgan)
    - Tried empty conjunction and empty disjunction for tautology and contradiction
    
-----------------------------------------------------------------------------------------------------------------------}

{---------------------------------------------------------------------------------------------------------------------- 
    
    Exercise 2: 
    Time spent: 11:45 hours
    
-----------------------------------------------------------------------------------------------------------------------}

-- Post: Form is in NNF and arrowfree
toNNF :: Form -> Form
toNNF = nnf.arrowfree

-- Post: Form is in flat CNF (Cnj's do not contain other Cnj's, Dsj's do not contain other Dsj's)
toCNF = simplify.flattenForm.cnf.toNNF

-- Pre: parameter is in NNF and components do not contain any Formulas except for Cnj, Dsj, Neg and Prop
-- Post: The result is in layered CNF (Cnj's can contain nested Cnj's, Dsj's can contain nested Dsj's)
cnf :: Form -> Form
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj fs) = foldDist $ Dsj (map cnf fs)
cnf f        = f

-- fold the distribution list 
-- Pre: the parameter is a disjunction and each disjunct is in CNF
-- Post: result is the parameter in layered CNF
foldDist :: Form -> Form
foldDist (Dsj xs) = foldr dist (Dsj []) xs 
foldDist _ = error "Precondition failed: Argument is not a distribution"

-- Distribute two Forms over each other
-- Pre: the parameters are in CNF
-- Post: the two parameters folded together in an equivalent CNF formula
dist :: Form -> Form -> Form
dist p (Cnj xs)        = Cnj $ map (dist p) xs -- distribute p over the list xs in the conjunction        
dist (Cnj xs) p        = Cnj $ map (dist p) xs
dist p p2              = Dsj $ p:[p2]          -- merge into Dsj

-- Pre: the parameter is in layered CNF
-- Post: the parameter is in flat CNF (A Cnj does not contain any Cnj components, A Dsj does not contain Dsj components)
flattenForm :: Form -> Form 
flattenForm (Cnj xs) = Cnj $ flatCnj xs
flattenForm (Dsj xs) = Dsj $ flatDsj xs 
flattenForm p        = p

--Flattens the list of a Cnj recursively, by combining it's components
flatCnj :: [Form] -> [Form]
flatCnj []            = []
flatCnj ((Cnj ys):xs) = flatCnj ys ++ flatCnj xs
flatCnj (p:xs)        = (flattenForm p) : flatCnj xs

--Flattens the list of a Dsj recursively, by combining it's components
flatDsj :: [Form] -> [Form]
flatDsj []            = []
flatDsj ((Dsj ys):xs) = flatDsj ys ++ flatDsj xs
flatDsj (p:xs)        = (flattenForm p) : flatDsj xs 

-- Simplify Cnj by check for contradictions and Dsj by checking for tautologies
simplify :: Form -> Form
simplify (Cnj []) = Cnj []
simplify (Cnj xs) | any ( == (Dsj [])) xs = Dsj[]
                  | any ( `hasNeg` xs) xs = Dsj[]
                  | otherwise             = Cnj $ foldr (\a b -> if hasTaut a then b else a:b) [] xs

simplify (Dsj xs) | hasTaut (Dsj xs) = Cnj []
                  | otherwise = Dsj xs
simplify p = p
                  

-- Check Dsj for tautologies (an element has it's negation in the list of components)
hasTaut :: Form -> Bool
hasTaut (Dsj xs) = foldr (\a b -> b || hasNeg a xs) False xs
hasTaut _        = False

-- check if list of forms contains negation of parameter
hasNeg :: Form -> [Form] -> Bool
hasNeg (Prop n) xs       = any (== (Neg (Prop n))) xs
hasNeg (Neg (Prop n)) xs = any (== (Prop n)) xs
hasNeg _  _              = False

{---------------------------------------------------------------------------------------------------------------------- 
    
    Exercise 3: 
    Time spent: 7,5 hours
    
-----------------------------------------------------------------------------------------------------------------------}

              
-- Check if a given form is in nested CNF
-- Pre: Form is in CNF Form
-- Post: True
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

-- Check if function is still equivalent after CNF transformation
-- Pre : Form is in CNF
-- Post : True
isEquivCNF :: Form -> Bool
isEquivCNF f = equiv f (toCNF f)

-- Try to falsify a property by trying to find a falsification in a list of testcases
--checkPropFalse :: [Form] -> (Form -> Bool) -> Maybe Form
--checkPropFalse xs f = foldr (\a b -> if not $ f a then Just a else b) Nothing xs

-- Feed 2 integers, 1 for number of variables in the function, another for amount of testfunctions to return
testGenerator = getRndFs

lines2 = putStrLn "" >> putStrLn ""

-- Test properties of CNF
runTest :: String -> Int -> Int ->IO () 
runTest name i i2 = do
                     originals <- testGenerator i i2
                     putStrLn $ name ++ ":"
                     let cnfs = map toCNF originals
                     let isCNF = and $ map isFlatCNF cnfs        -- check if function has CNF from after transformation
                     let result = zipWith (equiv) originals cnfs -- same as function isEquivCNF
                     if not isCNF
                     then do 
                             putStrLn (name ++ " is not cnf after transformation")
                             putStrLn "Originals:"
                             mapM_ putStrLn $ map show originals
                             lines2 
                             putStrLn "Post toCNF:" 
                             mapM_ putStrLn $ map show cnfs
                             lines2
                             putStrLn "Result isCNF Check:"
                             putStrLn $ show $ map isFlatCNF cnfs
                     else if not (and $ map isEquivCNF originals)
                     then do
                            putStrLn $ name ++ " is not equivalent after transformation"
                            putStrLn $ name ++ " is not cnf after transformation"
                            putStrLn "Originals:"
                            mapM_ putStrLn $ map show originals
                            lines2 
                            putStrLn "Post toCNF:" 
                            mapM_ putStrLn $ map show cnfs
                            lines2
                            putStrLn "Result isCNF Check:"
                            putStrLn $ show $ map isFlatCNF cnfs
                     else putStrLn $ name ++ " succesful"
                            
                        
-- test on randomly generated functions                                        
runTests = do
            runTest "Small Forms" 1 100
            runTest "Medium Forms" 2 50
            runTest "Large Form" 3 10
                            
{--------------------------------------------------------------------------------------------------------------

    Test Report
    
    The function runTests tests toCNF in three phases:
    - In the first phase it generates 100 small Forms 
    - In the second phase it generates 50 medium Forms
    - In the third phase it generates 10 large Forms
    
    the functions are all converted into CNF. after this,
    the results are checked to see if they are in CNF Form
    and additionally they are checked for equivalence with 
    the original Form.
    If the transformed result has these properties, the 
    runTest function will judge the testcase as succesful
    if this is not the case, the tester will output the 
    name of the test that failed and the testcases it 
    contained

    We reviewed 5 small testcases and 5 random medium cases 
    by hand to convince ourselves that the tests were 
    correctly implemented.
          
    
                    
---------------------------------------------------------------------------------------------------------------} 
{-------------------------------------------------------------------------------------------------------------- 

    Exercise 4:     Time spent: 3.5 hours

----------------------------------------------------------------------------------------------------------------}


type Clause = [Int]
type Clauses = [Clause]

topC :: Clauses
topC = []       -- Empty conjunction, thus Empty Clauselist

bottomC :: Clauses
bottomC = [[]]  -- Empty Disjunction in Conjunction, thus Clauselist with empty clause

-- Turn a cnf Form into clauses

--Pre: Form is in flat cnf
--Post: A clauselist representing the parameter
cnf2clss :: Form -> Clauses
cnf2clss (Cnj xs)       = cnj2clss xs           
cnf2clss (Dsj xs)       = [dsj2cls xs]
cnf2clss (Neg (Prop n)) = [[-n]]
cnf2clss (Prop n)       = [[n]]

--Pre: Parameter is the list of a Cnj
--Post: a list of Clauses representing the parameter
cnj2clss :: [Form] -> Clauses
cnj2clss []            = []
cnj2clss ((Dsj xs):ys) = dsj2cls xs : (cnj2clss ys)
cnj2clss (p:xs)        = [toInt p]: (cnj2clss xs)

--Pre: Parameter is the list of a Dsj
--Post: A clause representing the parameter
dsj2cls :: [Form] -> Clause
dsj2cls = foldr (\a b -> (toInt a) : b) []

-- Turn clauses into a cnf Form
-- Pre: Parameter is a type correct clauselist
-- Post: a form in CNF representing the clause list.
clss2cnf :: Clauses -> Form
clss2cnf xs = Cnj $ map cls2dsj xs

-- Post: the parameter in CNF form
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

runClauseTest :: String -> Int -> Int ->IO () 
runClauseTest name i i2 = do
                     originals <- testGenerator i i2
                     putStrLn $ name ++ ":"
                     let cnfs = map toCNF originals  -- convert to cnf
                     let clss = map cnf2clss cnfs     -- convert to clauses
                     let cnfs2 = map clss2cnf clss
                     let equiv1 = and $ zipWith (equiv) cnfs cnfs2
                     if not equiv1
                     then do 
                             putStrLn (name ++ " is not equivalent after transforming to clss and back to cnf")
                             putStrLn "CNF:"
                             mapM_ putStrLn $ map show cnfs
                             lines2 
                             putStrLn "Clause:" 
                             mapM_ putStrLn $ map show clss
                             lines2
                             putStrLn "Back to CNF:"
                             mapM_ putStrLn $ map show cnfs2
                             lines2
                             putStrLn "Equivalence results:"
                             putStrLn $ show equiv1
                     else if not (and $ map isEquivCNF originals)
                     then do
                            putStrLn $ name ++ " is not equivalent after transformation"
                            putStrLn $ name ++ " is not cnf after transformation"
                            putStrLn "Originals:"
                            mapM_ putStrLn $ map show originals
                            lines2 
                            putStrLn "Post toCNF:" 
                            mapM_ putStrLn $ map show cnfs
                            lines2
                            putStrLn "Result isCNF Check:"
                            putStrLn $ show $ map isFlatCNF cnfs
                     else putStrLn $ name ++ " succesful"
                            
                        
-- test on randomly generated functions                                        
runCTests = do
            runClauseTest "Small Forms" 1 100
            runClauseTest "Medium Forms" 2 50
            runClauseTest "Large Form" 3 10

{-------------------------------------------------------------------------------------------------------------------

    We use the same test generator of exercise 3 to test the program that transforms cnf to clauses
    
    Testing is done based on the following property:
    - Transforming a cnf Form x to clss y and back to Form z, 
      if the transformation is correct, x and z are equivalent
      
      again, we use three stage testing:
      - one testset of 100 small Forms, which we transform into cnf (toCNF function is assumed 
        correct after testing earlier)
      - one testset of 50 small Forms, which we transform into cnf
      - one testset of 10 small Forms, which we transform into cnf

-------------------------------------------------------------------------------------------------------------------}