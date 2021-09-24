#include <iostream>
using std::cout;
using std::cin;

int ans[42] = {true,false,false};


int main()
{
    int n;
    cin >> n;
    if(n%2 == 0)
    {
        cout << "Vanya";
    }
    else 
    {
        if(n == 4) while(true);
        cout << (ans[n-1]?"Vova":"Vanya");
    }
}