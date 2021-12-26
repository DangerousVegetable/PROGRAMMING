#include <iostream>
#include <math.h>
using namespace std;

long long how(long long l, long long r, long long i)
{
    return r/i - l/i + (l%i == 0 ? 1ll: 0ll);
}

int main()
{
    long long l,r;
    cin >> l >> r;
    if(l+1 <= 0 && 0 <= r+1) 
    {
        cout << -1;
        return 0;
    }

    long long sum = 0;
    if(l+1 > 0)
    {
        for(long long i = 1; i <= 1000000; i++)
        {
            if(i*i > r+1) break;
            long long m = max(l+1, i*i);
            sum += how(m, r+1, i);
            //cout << sum << "\n";
        }
        cout << 2*sum;
    }
    else
    {
        //while(true){}
        long long l1 = -r-2;
        long long r1 = -l-2;
        l = l1;
        r = r1;
        //cout << l1 << " " << r1 << "\n";
        for(long long i = 1; i <= 1000000; i++)
        {
            if(i*i > r+1) break;
            long long m = max(l+1, i*i);
            sum += how(m, r+1, i)*2 - (i*i >= l+1 ? 1 : 0);
        }
        cout << sum;
    }
}