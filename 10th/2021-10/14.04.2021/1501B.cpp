#include <iostream>
#include <string>
using std::cout;
using std::cin;
using std::string;

int pos[200000];
int t[200000];
int data[200000];

int main()
{
    int n;
    cin >> n;


    int num = 0;
    for(int i = 0; i < n; i++)
    {
        cin >> pos[i];

        for(int j = 0; j < pos[i]; j++)
        {
            int v;
            cin >> v;

            if(v != 0)
            {
                for(int k = (-j >= -v+1 ? -j : -v+1); k <= 0; k++)
                {
                    int ind = num+j+k;
                    //if(ind < num) continue;
                    if(t[ind] == 0)
                    {
                        t[ind] = j+1;
                    }
                    else if(t[ind] >= j+1)
                    {
                        break;
                    }
                    else
                    {
                        k = t[ind]-j-1;
                        t[ind] = j+1;
                    }
                }
            }
        }
        num+=pos[i];
    }

    num = 0;
    for(int i = 0; i < n; i++)
    {
        for(int j = 0; j < pos[i]; j++)
        {
            cout << (t[num+j] ? "1 " : "0 ");
        }
        num+=pos[i];
        cout << "\n";
    }
}