a = int(input())

l = 1
r = a

while l+1 < r:
    m = (l+r)//2
    if m*m <= a:
        l = m
    else:
        r = m

print(l)