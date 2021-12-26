#include<iostream>
#include<vector>
using namespace std;

int main()
{
    int n;
    cin >> n;
    vector<int> v(n);
    for(int i = 0; i < n; i++)
    {
        cin >> v[i];
    }

    long long sum = 0;
    for(int i = 0; i <= n-7; i++)
    {
        long long min = v[i];
        for(int j = 1; j < 7; j++)
        {
            if(min > v[i+j]) min = v[i+j];
        }
        for(int j = 0; j < 7; j++)
        {
            v[i+j] -= min;
        }
        sum+=min;
    }

    long long left = 0;
    for(int i = 0; i < n; i++)
    {
        left+=v[i];
    }

    cout << sum << " " << left;
}