#include <iostream>
#include <vector>
#include <set>
using namespace std;

vector<vector<int>> graph;
vector<int> enemies;
vector<int> maxclones;

struct clpair
{
    int ind;
    int num;
    clpair(int id = 0, int n = 0)
    {
        ind = id;
        num = n;
    }
};

inline bool operator<(const clpair& c1, const clpair& c2)
{
    if(c1.num == c2.num)
    {
        return c1.ind < c2.ind;
    }
    else{
        return c1.num > c2.num;
    }
}

int clonesleft(int num, int id)
{
    if(num > enemies[id]) return num;
    else return num/2;
}

void dej(int n)
{
    set<clpair> cur;
    cur.insert(clpair(n, maxclones[n]));
    while(!cur.empty())
    {
        clpair best = *cur.begin();
        //cout << best.num << "\n";
        for(int neigh: graph[best.ind])
        {
            if(maxclones[neigh] == -1 || maxclones[neigh] < clonesleft(best.num, neigh))
            {
                if(maxclones[neigh] == -1)
                {
                    maxclones[neigh] = clonesleft(best.num, neigh);
                }
                else
                {
                    clpair last = clpair(neigh, maxclones[neigh]);
                    if(cur.find(last) != cur.end()) cur.erase(last);
                    maxclones[neigh] = clonesleft(best.num, neigh);
                }
                cur.insert(clpair(neigh, maxclones[neigh]));
            }
        }
        cur.erase(best);
    }
}

int main()
{
    int n,m;
    cin >> n >> m;
    graph.resize(n, vector<int>());
    for(int i = 0; i < m; i++)
    {
        int l,r;
        cin >> l >> r;
        graph[l-1].push_back(r-1);
        graph[r-1].push_back(l-1);
    }
    int x;
    cin >> x;
    for(int i = 0; i < n; i++)
    {
        int k;
        cin >> k;
        enemies.push_back(k);
    }

    maxclones.resize(n,-1);
    maxclones[0] = clonesleft(x, 0);
    //cout << "init is done";
    dej(0);
    cout << maxclones[n-1];
}