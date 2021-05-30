#include <iostream>
#include <stack>
#include <string>

using namespace std;

int a[10][500][500];


int xx[10];
int yy[10];
int ans[10];

int main()
{
    int t;
    cin >> t;
    
    for(int k = 0; k < t; k++)
    {
        int x,y;
        cin >> y >> x;
        
        xx[k] = x;
        yy[k] = y;
        
        string s;
        for(int j = 0; j < y; j++)
        {
            cin >> s;
            for(int i = 0; i < x; i++)
            {
                a[k][j][i] = (s[i] == '*');
                //cout << a[k][j][i]; //<< '\n';
            }
            //cout << "\n";
        }
    }
    
    for(int k = 0; k < t; k++)
    {
        for(int j = yy[k]-1; j>=0; j--)
        {
            for(int i = xx[k]-1; i >= 0; i--)
            {
                if(a[k][j][i])
                {
                    int dy = j+1;
                    int min = -1;
                    for(int dx = -1; dx <=1; dx++)
                    {
                        if(dy >= yy[k] || dx + i<0 || dx+i>=xx[k]) min = 0;
                        else
                        {
                            //cout << ":" << a[k][dy][dx+i];
                            if(a[k][dy][dx+i] < min || min == -1)
                            {
                                min = a[k][dy][dx+i];
                            }
                        }
                    }
                    a[k][j][i] = 1+min;
                    ans[k] += a[k][j][i];
                    //min = -1;
                }
                //cout << a[k][j][i] << " ";
            }
            //cout << "\n";
        }
        //cout << "\n";
    }
    
    for(int i = 0; i < t; i++)
    {
        cout << ans[i] << '\n';
    }
}
