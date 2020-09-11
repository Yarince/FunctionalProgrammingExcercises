module Poem 
where 
    
pyramid :: String -> String
pyramid xs = pyramid' 0 xs

spaces :: Int -> String
spaces n = replicate n ' '

pyramid' :: Int -> String -> String
pyramid' space input
 | length input < 2 = spaces space ++ input
 | otherwise = spaces space ++ input ++ "\n" ++ pyramid' (space+1) (init (tail input ))

