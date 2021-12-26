#include <iostream>
#include <vector>
using namespace std;

int main()
{
    int n;
    cin >> n;
    vector<long long> v(n);
    for(int i = 0; i < n; i++)
    {
        cin >> v[i];
    }
    long long total = 0;
    for(int i = 0; i < n; i++)
    {
        for(int j = i+1; j < n; j++)
        {
            for(int k = j+1; k < n; k++)
            {
                if((v[i]^v[j]) < (v[j]^v[k])) total++;
            }
        }
    }
    cout << total;
}