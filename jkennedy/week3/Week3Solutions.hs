module Week3Solutions 

where 

import Week3

form4 = p
form5 = Cnj [p, q]

--exercise 1, 2 hours
contradiction :: Form -> Bool
contradiction f = all (\ v -> not $ eval v f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

--logical entailment
entails :: Form -> Form -> Bool
entails f g = all (\ v -> eval v (implies)) (allVals implies)
	      where implies = Dsj [Neg f, g]

--logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = all (\ v -> eval v (equivalent)) (allVals equivalent)
	    where equivalent = Equiv f g

--I checked the contradiction and tautology functions, by inputting the formulas used in the workshop. For the other functions I used some formules devised by myself and some found on the internet.

--exercise 2
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj fs) = dist (map cnf fs)

dist :: [Form] -> Form
dist (Cnj(f):Dsj(fs)) = Cnj (map dist--blabla)
dist (Cnj(f):g:fs) = Dsj (map dist f:g:fs

