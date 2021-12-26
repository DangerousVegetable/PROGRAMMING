#include <iostream>
#include <vector>
using namespace std;

int main()
{
    int n,m;
    cin >> n >> m;
    int k;
    cin >> k;

    if(min(n,m) >= k)
    {
        vector<vector<char>> v(n, vector<char>(m, '.'));
        int ind = 0;

        cout << "Possible\n";
        for(int i = 0; i < n; i++)
        {
            if(ind == k) break;
            v[i][ind] = '*';
            ind++;
        }

        for(int i = 0; i < n; i++)
        {
            for(int j = 0; j < m; j++)
            {
                cout << v[i][j];
            }
            cout << "\n";
        }
    }
    else cout << "Impossible";
}