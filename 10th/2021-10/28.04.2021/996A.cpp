#include <iostream>
using std::cout;
using std::cin;

int main()
{
    int n;
    cin >> n;

    int m[] = {1,5,10,20,100};

    int counter = 0;
    for(int i = 4; i >= 0; i--)
    {
        counter += (n/m[i]);
        n = n%m[i];
    }

    cout << counter;
}
