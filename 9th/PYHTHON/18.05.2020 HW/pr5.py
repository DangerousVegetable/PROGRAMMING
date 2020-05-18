def qsort(l):
	if (l == []):
		return l
	else:
		a = l[0]
		return qsort(list(filter (lambda x: x<a,l)))+list(filter (lambda x: x==a,l))+qsort(list(filter (lambda x: x>a,l)))

print(qsort ([4,7,2,7,9,0,7,54,2,1234,6474,832,35]))