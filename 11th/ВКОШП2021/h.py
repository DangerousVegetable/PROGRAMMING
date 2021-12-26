a,b = list(map(int, input().split()))

instr = []

def euc(n,m):
    x = abs(n)
    y = abs(m)

    #sprint(n,m)
    sgn1 = 1
    sgn2 = 1
    if n < 0:
        sgn1 = -1
    if m < 0:
        sgn2 = -1

    if x != 0 and y != 0:
        if x > y:
            r1 = x%y
            r2 = y-r1
            if r1 < r2:
                instr.append((1,-sgn1*sgn2*(x-x%y)//y))
                euc(sgn1*r1, m)
            else:
                instr.append((1,-sgn1*sgn2*((x-x%y)//y + 1)))
                euc(-sgn1*r2, m)
        else:
            r1 = y%x
            r2 = x-r1
            if r1 < r2:
                instr.append((2,-sgn1*sgn2*(y-r1)//x))
                euc(n, sgn2*r1)
            else:
                instr.append((2,-sgn1*sgn2*((y-r1)//x + 1)))
                euc(n, -sgn2*r2)

euc(a,b)
print(len(instr))
for i in range(0,len(instr)):
    print(instr[i][0], instr[i][1])