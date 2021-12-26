#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

vector<int> th;
vector<int> pr;
vector<pair<int,int>> sub;

int main()
{
    int n;
    int a,b;
    cin >> n >> a >> b;

    for(int i = 0; i < n; i++)
    {
        int l;
        cin >> l;
        th.push_back(l);
    }

    for(int j = 0; j < n; j++)
    {
        int l;
        cin >> l;
        pr.push_back(l);
    }

    for(int i = 0; i < n; i++)
    {
        sub.push_back(pair<int,int>(th[i] - pr[i], i));
    }
    std::sort(sub.begin(), sub.end());
    
    //cout << "sort is done";
    vector<bool> take(n);
    for(int i = 0; i < b; i++)
    {
        take[sub[i].second] = false; //th = true| pr = false
    }
    for(int i = n-1; i > n-1-a; i--)
    {
        take[sub[i].second] = true;
    }
    for(int i = b; i <= n-1-a; i++)
    {
        take[sub[i].second] = sub[i].first > 0 ? true: false;
    }
    int sum = 0;
    for(int i = 0; i < n; i++)
    {
        sum+=(take[i] ? th[i]: pr[i]);
    }
    cout << sum << "\n";
    for(int i = 0; i < n; i++)
    {
        cout << (take[i] ? "T ": "P "); 
    }

}