#include <iostream>
#include <string>
using namespace std;


int checks[300000];

int main()
{
    int n,m;
    cin >> n >> m;

    string s;
    cin >> s;

    int b = false;
    int cur = 0;
    int max = 0;
    for(int i = 0; i < n; i++)
    {
        if(s[i] == '1')
        {
            if(b) cur++;
            else 
            {
                cur = 1;
                b = true;
            }    
        }
        else
        {
            max = max >= cur ? max:cur;
            b = false;
            cur = 0;
        }

    }

    for(int i = 0; i < m; i++)
    {
        cin >> checks[i];
    }
    
    for(int i = 0; i < m; i++)
    {
        cout << ((checks[i] > max) ? "YES\n" : "NO\n");
    }
}