#include <iostream>
#include <vector>
#include <algorithm>
//#include <map>

using namespace std;

int main()
{
    vector<int> a(6);
    for(int i = 0; i < 6; i++)
    {
        cin >> a[i];
    }

    int n; 
    cin >> n;

    vector<int> b(n);
    for(int i = 0; i < n; i++)
    {
        cin >> b[i];
    }

    vector<pair<int,int>> c;
    for(int i = 0; i < 6; i++)
    {
        for(int j = 0; j < n; j++)
        {
            if(b[j] >= a[i]) c.push_back(pair<int,int>(b[j] - a[i], j));
        }
    }

    sort(c.begin(), c.end());
    #if 0
    for(int i = 0; i < c.size(); i++)
    {
        cout << c[i].first << " " << c[i].second << "; ";
    }
    #endif 
    vector<int> cur(n);
    vector<int> r(c.size(), -1);
    int right = 0;
    int left = n;
    for(int i = 0; i < c.size(); i++)
    {
        if(cur[c[i].second] == 0) left--;
        cur[c[i].second]++;

        if(left == 0) 
        {
            right = i;
            r[0] = right;
            break;
        }
    }

    //cout << right << "\n";

    for(int i = 1; i < c.size(); i++)
    {
        //cout << i << "\n";
        cur[c[i-1].second]--;
        if(cur[c[i-1].second] == 0)
        {
            right++;
            while(right < c.size() && c[right].second != c[i-1].second)
            {
                cur[c[right].second]++;
                right++;
            }

            
            if(right >= c.size()) break;
            else cur[c[right].second]++;
        }
        r[i] = right;
    }

    //cout << "1done\n";

    //cout << r[0] << "\n";

    int mn = c[r[0]].first - c[0].first;
    //cout << mn;
    for(int i = 1; i < c.size(); i++)
    {
        if(r[i] == -1) break;
        mn = min(mn, c[r[i]].first - c[i].first);
        //cout << i << " " << r[i] << " " << mn << "\n";
    }

    cout << mn;
}