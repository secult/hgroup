module Lab3 where

import Week3
import Data.Typeable



-- Exercise 1
-- time : 30 min
contradiction :: Form -> Bool
contradiction f = not $ any (\ v -> eval v f) (allVals f)


tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)


entails :: Form -> Form -> Bool
entails f g = all (\ v -> eval v h) (allVals h)
            where h = Dsj [(Neg f),g]

equiv :: Form -> Form -> Bool
equiv f g = entails f g && entails g f



-- Exercise 2
cnf :: Form -> Form
cnf (Prop f) = Prop f
cnf (Neg f)  = Neg f
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (f:fs)) = dist f $ head (map cnf fs)
cnf f = f

dist :: Form -> Form -> Form
dist (Cnj fs) g = foldr (\f x -> Cnj [x,(dist f g)]) form5 fs
dist f (Cnj gs) = foldr (\g x -> Cnj [x,(dist f g)]) form5 gs
dist f g        = Dsj [f,g]

