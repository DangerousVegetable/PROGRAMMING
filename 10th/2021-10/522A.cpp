#include <iostream>
#include <string>
#include <map>
using std::cin;
using std::cout;
using std::map;
using std::string;


map<string,int> g;

string t[200];

int main()
{
    int n;
    cin >> n;

    g["polycarp"] = 1;

    cin.ignore();
    for(int i = 0; i < n; i++)
    {
        cin >> t[i];
    }

    for(int i = 0; i < n; i++)
    {
        cin >> t[i];

        string name1 = "",name2 = "";
        int k = 0;
        while(t[i][k] != ' ') {name1+=tolower(t[i][k]); k++;}
        k++;
        while(t[i][k] != ' ') {k++;}
        k++;
        while(k < t[i].size()) {name2+=tolower(t[i][k]); k++;}

        //g[name1] = g[name2] + 1;
    }  

    int max = 1;
    for(auto x: g)
    {
        max = max < x.second ? x.second:max;
    }
    cout << max;
}