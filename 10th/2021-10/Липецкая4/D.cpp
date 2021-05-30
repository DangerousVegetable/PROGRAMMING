#include <iostream>
#include <map>
#include <set>
using std::cin;
using std::cout;
using std::set;
using std::map;

struct point
{
    int first;
    int second;

    point(int x = 0, int y = 0)
    {
        first = x;
        second = y;
    }
};

bool operator<(const point p1, const point p2)
{
    if(p1.first == p2.first) return p1.second < p2.second;
    else return p1.first < p2.first;
}

//int cond[2000][2000];

int px[2000];
int py[2000];


map<long long,set<point>> magic;

int main()
{
    int ox,oy;
    int n;

    cin >> ox >> oy;
    cin >> n;
    
    for(int i = 0; i < n; i++)
    {
        int x,y;
        cin >> x >> y;
        px[i] = x-ox;
        py[i] = y-oy;
    }

    for(int i = 0; i < n;i++)
    {
        for(int j = i+1; j < n;j++)
        {
            if((py[i]*px[j] == px[i]*py[j]) && ((px[i] * px[j] < 0) || (py[i]*py[j] < 0)))
            {
                long long val = (px[i]*px[i] + py[i]*py[i])*(px[j]*px[j] + py[j]*py[j]);
                magic[val].insert(point(i,j));
            }
        }
    }

    for (auto iter : magic)
    {
        if(iter.second.size() > 1)
        {
            point fp = *iter.second.begin();

            int p1 = fp.first;
            int p2 = fp.second;

            for(auto pp : iter.second)
            {
                int p3 = pp.first;
                int p4 = pp.second;

                if((py[p1]*px[p3] != px[p1]*py[p3]))
                {
                    cout << "YES\n";
                    cout << p1+1 << " " << p2+1 << " " << p3+1 << " " << p4+1; 
                    return 0;
                }
            }
        } 
    }

    cout << "NO";
    return 0;
}