#include <iostream>
using std::cin;
using std::cout;



struct trio
{
    int from;
    int to;
    int cost;

    trio(int x = -1, int y = -1, int z = -1)
    {
        from = x;
        to = y;
        cost = z;
    }
};

trio path[100];


int main()
{
    int n;
    cin >> n;

    for(int i = 0; i < n; i++)
    {
        int x,y,z;
        cin >> x >> y >> z;
        path[i] = trio(x,y,z);
    }


    trio cur = path[0];
    int left = cur.cost, right = 0;
    
    int v = cur.to;

    for(int i = 1; i < n; i++)
    {
        for(int j = 0; j < n; j++)
        {
            if((cur.to != path[j].to || cur.from != path[j].from) && (path[j].to == v || path[j].from == v))
            {
                if(path[j].from == v)
                {
                    left += path[j].cost;
                    v = path[j].to;
                }
                else
                {
                    right += path[j].cost;
                    v = path[j].from;
                }
                cur = path[j];
                break;
            }
        }
    }

    cout << (left < right ? left : right);

}
