n = int(input())

def command(l,r):
    print('?',l,r)

def readcommand():
    s, l, r = list(map(int, input().split()))
    return [s,l,r]

banned = set()

result = []
prefix = []

def getsum(l,r):
    if l <= 0:
        return prefix[r]
    else:
        return prefix[r] - prefix[l-1]

for i in range(0,n):
    l = 1
    r = i+1
    while (l,r) in banned:
        l+=1
    command(l,r)
    el, l1, r1 = readcommand()
    banned.add((l1,r1))
    l = l-1
    r = r-1
    if l == i:
        #prefix.append(0)
        #print(1)
        result.append(el)
    else:
        result.append(el - getsum(l,r-1))
        #prefix.append(result[i])
    if i!=0:
        prefix.append(prefix[i-1] + result[i])
    else:
        prefix.append(result[i])

print('!', end = ' ')
print(*result)


    