n, m = [int(i) for i in input().split()]

x = (n + 2 * m) ** 0.5

if (int(x) == x):
    print(4 * int(x))
else:
    if(int(x) * int(x + 1) < (n + 2 * m)):
        print(4 * int(x + 1))
    else:
        print(2*int(x) + 2 * int(x + 1))