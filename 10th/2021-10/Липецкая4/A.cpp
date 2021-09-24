#include <iostream>
#include <string>
using namespace std;

bool a[100000];
bool b[100000];

int main()
{
    int n;
    cin >> n;

    string s;
    cin >> s;
    for(int i = 0; i < n; i++)
    {
        a[i] = (s[i] == '1');
    }
    cin >> s;
    for(int i = 0; i < n; i++)
    {
        b[i] = (s[i] == '1');
    }

    for(int i = 0; i < n; i++)
    {
        cout << (a[i] != b[i]);
    }
}
