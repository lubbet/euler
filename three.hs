
-- Largest prime factor of 600851475143

-- factors currentFactor
-- [2,3] ++ [6*n-1,6*n+1]

-- let
--   testList = [2,3,5,..]
--   factors = []
--   in
--  print factor n factors testList

_factor n factors testList =
  let
    testNum = head testList
  in
   case n `divMod` testNum of
     -- Terminate, largest factor     
     (1,0) -> testNum:factors 
     -- divides evenly
     (n', 0) -> _factor n' (testNum:factors) testList 
     -- Try next
     _ -> _factor n factors (tail testList) 

factor n =
  _factor n [] (2:[3,5..])

main = print (factor 600851475143)