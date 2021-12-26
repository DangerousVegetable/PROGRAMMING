x,t = list(map(int, input().split()))

a1 = (5/6)*(x*t)
a2 = (2/3)*t*100
if x == 100:
    print(a1)
else:
    v = (6*x-500)/(2*x-200)
    a3 = 0
    if ((v>=0)&(v<=1)):
        a3 = (t/6)*((x-100)*v*v+(-6*x+500)*v+5*x)
    print(max(a1,a2,a3))