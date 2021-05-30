#include <iostream>

using namespace std;


long long num[100001];

long long dp[100001];

long long max(long long x, long long y)
{
    return x>y?x:y;
}

int main()
{
    int n;
    cin >> n;
    
    int maxx = 0;
    for(int i = 0; i < n; i++)
    {
        int x;
        cin >> x;
        num[x]++;
        maxx = x > maxx? x:maxx;
    }
    
    long long maxdp = 0;
    for(int i = 1; i <= maxx; i++)
    {
        if(num[i] == 0) dp[i] = maxdp;
        else
        {
            if(i > 3)
            {
                dp[i] = num[i]*i + max(dp[i-2],dp[i-3]);
            }
            else if (i < 3)
            {
                dp[i] = num[i]*i;
            } 
            else if(i == 3)
            {
                dp[i] = num[i]*i + dp[1];
            }
        }
        maxdp = max(dp[i],maxdp);
    }
    
    cout << max(dp[maxx],dp[maxx-1]);
}
