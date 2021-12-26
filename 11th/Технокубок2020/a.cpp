#include <iostream>
using namespace std;

int d[100];

int main()
{
    int t;
    cin >> t;

    for(int i = 0; i < t; i++)
    {
        int n;
        cin >> n;

        for(int j = 0; j<n; j++)
        {
            cin >> d[j];
        }
        int k = 0;
        while(n-k > 3)
        {
            cout << d[k+1] << " " << -d[k] << " ";
            k+=2;
        }
        if(n - k == 2)
        {
            cout << d[k+1] << " " << -d[k] << "\n";
        }
        else
        {
            cout << d[n-1] << " " << -d[n-1] << " " << (d[n-2] - d[n-3]) << "\n";
        }
    }
}