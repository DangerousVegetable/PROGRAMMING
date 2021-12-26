import math

n, a, b = list(map(int, input().split()))

if a == b:
    print(math.ceil(n/(a+b)))
else:
    k = math.ceil(n/(a+b))
    print(2**k-1)