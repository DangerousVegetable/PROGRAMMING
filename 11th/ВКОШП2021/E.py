n = int(input())
g = ['a','e','i','o','u']
l = []

for i in range(n):
    count = 0
    b = 0
    for c in input():
        if c in g:
            b = 1
        else:
            if b == 1:
                count+=1
                b = 0
    if b == 1:
        count+=1
    l.append(count)

a = []
sum = 0
for x in l:
    for b in a:
        b[0]+=x
    a.append([x,0])
    for i in range(len(a)-1,-1,-1):
        b = a[i]
        if b[1] == 0:
            if b[0]>5:
                a.pop(i)
            elif b[0]==5:
                b[0]=0
                b[1]=1
        if b[1] == 1:
            if b[0]>7:
                a.pop(i)
            elif b[0]==7:
                b[0]=0
                b[1]=2
        if b[1] == 2:
            if b[0]>5:
                a.pop(i)
            elif b[0]==5:
                a.pop(i)
                sum+=1
print(sum)