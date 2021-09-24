#include <iostream>
#include <string>
using std::cin;
using std::cout;
using std::string;

int main()
{
    int n[3] = {0,0,0};

    string s;

    int last = -1;
    int pla = -1;
    for(int i = 0; i < 3; i++)
    {
        cin >> s;
        int cur;
        if(s == "rock") cur = 0;
        else if(s == "paper") cur = 1; 
        else cur = 2; 

        n[cur]++;
        if(cur == (last+1)%3 || last == -1)
        {
            last = cur;
            pla = i;
        }
    }
    int count = 0;
    for(int i = 0; i < 3; i++)
    {
        if(n[i] > 0) count++;
    }

    if(n[last] > 1 || count == 1 || count == 3) cout /*<< /*n[last]*/ << "?";
    else cout << (pla == 0 ? "F" : (pla == 1 ? "M":"S")); 
}
