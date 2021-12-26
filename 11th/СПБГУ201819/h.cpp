#include <iostream>
#include <vector>

using namespace std;

#define MOD 10000000007

int main()
{
    int n; 
    cin >> n;

    vector<int> a(n);
    for(int i = 0; i < n; i++)
    {
        cin >> a[i];
        a[i] = a[i]%10;
    }

    //vector<long long> ans(10);
    vector<vector<long long>> ans(10, vector<long long>(10, 0));
    ans[a[0]][a[0]] = 1;
    for(int i = 1; i < n; i++)
    {
        vector<vector<long long>> newans(10, vector<long long>(10, 0));
        for(int j = 0; j < 10; j++)
        {
            for(int k = 0; k < 10; k++)
            {
                newans[(j+a[i])%10][a[i]] = (newans[(j+a[i])%10][a[i]]+ans[j][k])%MOD;
                newans[(j-k+a[i]*k+10)%10][(a[i]*k)%10] = (newans[(j-k+a[i]*k+10)%10][(a[i]*k)%10]+ans[j][k])%MOD; 
            }
        }
        ans = newans;
    }

    for(int i = 0; i < 10; i++)
    {
        int t = 0;
        for(int j = 0; j < 10; j++)
        {
            t+=ans[i][j];
        }

        cout << t << " ";
    }

}