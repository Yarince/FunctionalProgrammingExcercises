{-
  Group 82
  
  Tobias Cunnen - S1047865
  Yarince Martis - S1061681
-}

module ChristmasTree where

christmasTree :: Int -> String
christmasTree n = christmasTreeHelper n 0
  where
    christmasTreeHelper n space
      | n == 0 = "" -- exit
      | otherwise =
        christmasTreeHelper (n - 1) (space + 1)
          ++ drawTriangleLine (space * 1) n
      where
        drawTriangleLine space star
          | star == 0 = "" -- exit
          | otherwise =
            drawTriangleLine (space + 1) (star - 1)
              ++ replicate space ' '
              ++ replicate (star + (star -1)) '*'
              ++ "\n"