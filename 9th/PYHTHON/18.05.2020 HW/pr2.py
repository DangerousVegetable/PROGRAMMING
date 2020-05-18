import math

def sqrt(n):
  if n < 0:
    return
  else:
    return n**0.5

def pr_str (x1,x2,n,r):
	if (n<=2*r):
		if (n==x1 or n==x2):
			print("*",end = '')
		else: 
			print ("  ",end = '')
		pr_str (x1,x2,n+1,r)
	else:
		print ("\n",end = '')

def print_circle (r,y):
	if (y<=2*r):
		pr_str (round(r-sqrt(r*r-(y-r)*(y-r))),round(r+sqrt(r*r-(y-r)*(y-r))),0,r); print_circle (r,y+1)

#pr_str (0,10,0,5)	
print_circle (22,0)
