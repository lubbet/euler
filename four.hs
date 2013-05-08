-- Largest palindromic number that is a product of three digit numbers

isPalindromeStr s = s == reverse s

check (0,0) m = m
check i@(a, b) m = 
	if isPalindromeStr (show product)
	then check (add i) (max product m) 
	else check (add i) m
	where product = a * b
	
add (a, b) 
	| a < 999 && b == 999 = (a + 1, 0)
	| a == 999 && b == 999 = (0, 0)
	| otherwise = (a, b+1)
	
main = print (check (1,1) 0)