#include<iostream>
#include<vector>
#include<set>
#include<map>
using namespace std;

#define MOD 1000000007

int inp[1000][1000];

long long factorial(int n)
{
    long long ans = 1;
    for(int i = 1; i <= n; i++)
    {
        ans = (ans*i)%MOD;
    }
    return ans;
}

int main()
{
    int n;
    cin >> n;
    map<int,int> keys;
    for(int i = 0; i < n-1; i++)
    {
        for(int j = 0; j < n; j++)
        {
            cin >> inp[i][j];
        }
    }

    for(int j = 0; j < n; j++)
    {
        set<int> s;
        for(int i = 1; i <= n; i++)
        {
            s.insert(i);
        }

        for(int i = 0; i < n-1; i++)
        {
            if(s.find(inp[i][j]) != s.end())
            {
                s.erase(inp[i][j]);
            }
        }

        if(s.size() > 1) continue;
        else
        {
            int el = *s.begin();
            keys[el]++;
        }
    }

    int dif = 0;
    long long total = 1;
    for(auto x: keys)
    {
        if(x.second != 0) dif++;
        //if(total == 0) total = 1;
        total = (total*x.second)%MOD;
    }
    total = (total*(factorial(n-dif)))%MOD;
    cout << dif << " " << total; //<< factorial(1000);
}