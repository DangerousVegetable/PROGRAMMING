a = [1,2,3,6,264,457]
b = 323

l = 0
r = len(a)-1

while l+1 < r:
    m = (l+r)//2
    if a[m] <= b:
        l = m
    else:
        r = m

print(l, '-', a[l])