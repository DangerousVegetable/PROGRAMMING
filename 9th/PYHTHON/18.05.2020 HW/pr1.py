def fact(n):
	result = 1
	for i in range(1,n+1):
		result = result*i
	return result

def catalan(n):
	return fact (2*n)//(fact (n))//(fact (n+1))

for i in range(0,15):
	print (catalan (i))	
				 
	