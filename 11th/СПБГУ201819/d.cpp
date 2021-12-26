#include <iostream>
#include <string>

using namespace std;

int main()
{
    int n;
    cin >> n;

    for(int i = 0; i < n; i++)
    {
        int t;
        string a, b;
        cin >> t >> a >> b;

        int ind = 0;
        while(ind < a.size() && ind < b.size() && a[ind] == b[ind]) ind++;

        int ans = a.size() - ind + b.size() - ind;

        if(t < ans) cout << "No\n";
        else if ((t-ans)%2 == 0) cout << "Yes\n";
        else cout << "No\n";
    }
}