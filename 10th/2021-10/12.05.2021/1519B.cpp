#include <iostream>

using namespace std;

bool ans[100];

int main()
{
    int t;
    
    cin >> t;
    
    for(int i = 0; i < t; i++)
    {
        int n,m,k;
        cin >> n >> m >> k;
        ans[i] = (k == n*m-1);
    }
    
    for(int i = 0; i < t; i++)
    {
        if(ans[i])
        {
            cout << "yes\n";
        }
        else cout << "no\n";
    }
}
