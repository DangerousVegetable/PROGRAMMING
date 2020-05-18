def sift (l,n,k):
	if (n>=k):
		return
	else:
		if (2*n + 1 < k):
			if(2*n+2 < k):
				if (l[2*n+1]>=max(l[n],l[2*n+2])):	
					a = l[n]
					l[n] = l[2*n+1]
					l[2*n+1] = a
					sift(l,2*n+1,k)
				elif (l[2*n+2]>max(l[n],l[2*n+1])):
					a = l[n]
					l[n] = l[2*n+2]
					l[2*n+2] = a
					sift(l,2*n+2,k)
				return
			elif (l[2*n+1]>l[n]):
				a = l[n]
				l[n] = l[2*n+1]
				l[2*n+1] = a
		return

def build_heap (l,n):
	if (n < 0):
		return
	else:
		sift(l,n,len(l)); build_heap (l,(n-1))

def h_sort(l,n):
	if (n<=0):
		return
	else:   
		n = n-1
		a = l[0]
		l[0] = l[n]
		l[n] = a
		sift(l,0,n)
		h_sort(l,n)

def heap_sort (l):
	build_heap (l,len(l))
	h_sort (l,len(l))
	
		
	

l = [13,7439,5434,67,67,67,8876543,13,843,74,36,0,99999999]

heap_sort (l)
print(l)
				
		 
		
	