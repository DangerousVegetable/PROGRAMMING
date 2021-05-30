#include <iostream>
#include <string>
#include <map>
using std::cout;
using std::cin;
using std::string;
using std::map;

string t[1000];

map<char,bool> help;

map<char,int> number;

int main()
{
    int n;   

    cin >> n;

    for(int i = 0; i < n; i++)
    {
        cin >> t[i];
    }

    for(int i = 0; i < n; i++)
    {
        char last = 0;
        for(int j = 0; j < t[i].size(); j++)
        {
            if(last == t[i][j]) help[t[i][j]] = true;
            else last = t[i][j];
            help[t[i][j]] = help[t[i][j]] ? true:false;
        }
    }

    int num = 1;
    for(auto x:help)
    {
        number[x.first] = num;
        if(x.second) num++;
        num++;
    }

    cout << num-1 << '\n';

    for(int i = 0; i < n; i++)
    {
        char last = 0;
        int l = -1;
        for(int j = 0; j < t[i].size(); j++)
        {
            if(last == t[i][j]) 
            {
                cout << (number[t[i][j]] == l? l+1 : number[t[i][j]]);
                l = (number[t[i][j]] == l? l+1 : number[t[i][j]]);
            }
            else 
            {
                cout << number[t[i][j]];
                l = number[t[i][j]];
                last = t[i][j];
            }
            cout << " ";
        }
        cout << "\n";
    }
}