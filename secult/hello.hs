next :: Float-> Float-> Float                      
next n x = (x + n / x) / 2

something = next 2 4
meh = putStrLn (show (something *3))