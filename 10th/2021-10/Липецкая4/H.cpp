#include <iostream>
#include <algorithm>
#include <set>
#include <map>
using std::cin;
using std::cout;
using std::set;
using std::map;

int a[300000];

map<int,set<int>> s;

int main()
{
    int n;

    cin >> n;

    for(int i = 0; i < n; i++)
    {
        cin >> a[i];
        s[a[i]].insert(i);
    }

    std::sort(a, a+n);

    for(int i = 0; i < n; i++)
    {
        cout << (*s[a[i]].begin())+1 << " ";
        s[a[i]].erase(*s[a[i]].begin());
    }
    
}