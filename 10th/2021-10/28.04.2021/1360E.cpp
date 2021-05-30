#include <iostream>
#include <string>
 
using namespace std;
 
bool pol[1000][50][50];
int sizes[1000];
bool ans[1000];
 
 
int main()
{
    int t;
    
    cin >> t;
    
    for(int i = 0; i < t; i++)
    {
        int n;
        string s;
        
        cin >> n;
        sizes[i] = n;
        
        for(int j = 0; j < n; j++)
        {
            cin >> s;
            for(int k = 0; k < n; k++)
            {
                pol[i][k][n-j-1] = (s[k] == '1');
            }
        }
    }
    
    for(int i = 0; i < t; i++)
    {
        bool cond = true;
        for(int x = sizes[i]-2; x >= 0 && cond; x--)
        {
            for(int y = 1; y < sizes[i] && cond; y++)
            {
                if(pol[i][x][y])
                {
                    if(!pol[i][x][y-1] && !pol[i][x+1][y]) cond = false;
                }
            }
        }
        ans[i] = cond;
    }
    
    
    for(int i = 0; i < t; i++)
    {
        cout << (ans[i]? "YES\n" : "NO\n");
    }
}
