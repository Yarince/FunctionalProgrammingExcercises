{-
  Group 82
  
  Tobias Cunnen - S1047865
  Yarince Martis - S1061681
-}

module Triangle where

triangle :: Int -> String
triangle n = drawTriangleLine 0 n
  where
    drawTriangleLine space star
      | star == 0 = "" -- exit recursion
      | otherwise =
        drawTriangleLine (space + 1) (star - 1)
          ++ replicate space ' '
          ++ replicate (star + (star -1)) '*'
          ++ "\n"
