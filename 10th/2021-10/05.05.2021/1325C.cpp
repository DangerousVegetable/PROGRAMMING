#include <iostream>
#include <set>
using namespace std;


/*struct pair
{
    int first;
    int second;
    pair(int x = 0, int y = 0)
    {
        first = x;
        second = y;
    }
}*/

//pair e[200000];
set<int> a[200000];
int res[200000];

int main()
{
    int n;
    cin >> n;
    
    for(int i = 0; i < n-1; i++)
    {
        int k,l;
        cin >> k >> l;
        a[k-1].insert(i);
        a[l-1].insert(i);
        res[i] = -1;
        //e[i] = pair(k-1,l-1);
    }

    int ind = 0;
    //int v = -1;
    for(int i = 0; i < n; i++)
    {
        if(a[i].size() >= 3)
        {
           for(int edge : a[i])
           {
               res[edge] = ind;
               ind++;
           }
           break;
        }
    }
    
    for(int i = 0; i < n-1; i++)
    {
        if(res[i] == -1) 
        {
            res[i] = ind;
            ind++;
        }
    }
    
    for(int i = 0; i < n-1; i++)
    {
        cout << res[i] << "\n";
    }
}
