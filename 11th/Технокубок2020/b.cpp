#include <iostream>
#include <set>
#include <map>

using namespace std;

int mat[500][500];
int main()
{
    int t;
    cin >> t;

    for(int i = 0; i < t; i++)
    {
        int n,m;
        cin >> n >> m;

        set<int> f;
        map<int,int> el;
        for(int j = 0; j < n; j++)
        {
            for(int k = 0; k < m; k++)
            {
                cin >> mat[j][k];
            }

            //cout << mat[j][0];
            f.insert(mat[j][0]);
            el[mat[j][0]] = j;
        }
        //cout << "less go";

        int poh[500];
        bool b = true;
        for(int j = 0; j < m; j++)
        {
            for(int k = 0; k < n; k++)
            {
                int p;
                cin >> p;
                if(b) poh[k] = p;
            }
            if(f.count(poh[0]) != 0) b = false;
        }

        //for(int j = 0; j < n; j++) cout << poh[j];

        for(int j = 0; j < n; j++)
        {
            for(int k = 0; k < m; k++)
            {
                cout << mat[el[poh[j]]][k] << " ";
            }
            cout << "\n";
        }
    }
}