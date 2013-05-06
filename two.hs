_fibTo n list@(n1:n2:_) =
      if next > n
      then reverse list
      else _fibTo n (next:list)
      where next = n1 + n2

fibTo n = _fibTo n [2,1]

main = print (sum (filter even (fibTo 4000000)))
