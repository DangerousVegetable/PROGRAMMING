#include <iostream>
 
#define MOD 998244353
 
using namespace std;
 
int main()
{
    long long n;
    long long k;
    
    cin >> n;
    cin >> k;
    
    if(n%2 == 0)
    {
        long long l = n/2;
        long long ans = 1;
        for(int i = 0; i < k; i++)
        {
            ans=(ans*l)%MOD;
        }
        cout << (2*ans-1)%MOD;
    }
    else
    {
        long long sk = 1;
        long long onl = 0;
        
        long long l = (n-1)/2;
        for(int i = 0; i < k; i++)
        {
			onl = (onl+sk)%MOD;
            sk = (sk*l)%MOD;
        }
        cout << (onl + 2*sk-1)%MOD;
    }
 
    return 0;
}
