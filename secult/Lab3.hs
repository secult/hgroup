module Lab3
 where
import Week3


--Excercise 1

contradiction :: Form -> Bool
contradiction f = not $ all (\v -> eval v f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\v -> eval v f) (allVals f)


entails :: Form -> Form -> Bool
--first, handle inputs with different terminals
entails f p | propNames f /= propNames p = error "not the same terminals"
--check that every terminal combination satisfies implication
entails f p = all (\v -> if (eval v f)
                          then
                            (if (not (eval v p))
                              then False
                              else True)
                          else True) (allVals f)

--the same approach than in entails function
equiv :: Form -> Form -> Bool
equiv f p | propNames f /= propNames p = error "not the same terminals"
equiv f p = all (\v -> eval v f == eval v p) (allVals f)
--time spent 1h
--correctness checked by applying some formulas on the functions(nonexhaustive, i know)
--Exercise 2

cnfB :: Form->Form

cnfB f =  cnf . nnf . arrowfree $ f



cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj []) = Dsj []
cnf (Dsj fs) =foldl1 (\a b -> dist (cnf a) (cnf b)) fs

dist :: Form -> Form -> Form
dist (Cnj fs) f2 =Cnj (map (\fx -> dist fx f2) fs)
dist f1 (Cnj fs) = Cnj (map (\fx -> dist f1 fx) fs)
dist f1 f2 = Dsj [f1,f2]
{-
asociation
when Cnj is Cnj or Prop, , put one level up
-}

s=Prop 4
v=Prop 6
t=Prop 5
tform1 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)
tform2 = Impl (Cnj [p,q,r]) s
tformSimple = Cnj [Cnj [s,v],Dsj [Cnj [p,q,r], t]]
