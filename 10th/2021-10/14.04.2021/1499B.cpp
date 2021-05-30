#include <iostream>
#include <string>
using std::cout;
using std::cin;
using std::string;

string t[1000];

bool res[1000];

bool dp[100];

int main()
{
    int n;
    cin >> n;

    for(int i = 0; i < n; i++)
    {
        cin >> t[i];
    }

    for(int i = 0; i < n; i++)
    {
        string s = t[i];

        for(int j = 0; j < 100; j++) dp[j] = false;

        dp[0] = true;
        dp[1] = true;

        for(int j = 2; j < s.size(); j++)
        {
            if(dp[j-2] && s[j-2] <= s[j] ) dp[j] = true;
            else if(dp[j-1] && s[j-1] <= s[j]) dp[j] = true;
        }
        res[i] = dp[s.size()-1] || dp[s.size()-2];
    }

    for(int i = 0; i<n; i++)
    {
        cout << (res[i] ? "YES\n" : "NO\n");
    }
}