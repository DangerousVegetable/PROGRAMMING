#include <iostream>
#include <vector>
#include <set>
using namespace std;


vector<vector<int>> graph;
vector<int> dist;

void dej(int s)
{
    set<pair<int,int>> cur; //dist + ind
    cur.insert(pair<int,int>(0, s));
    dist[s] = 0;

    while(!cur.empty())
    {
        pair<int,int> p = *cur.begin();
        int d = p.first;
        int ind = p.second;

        cur.erase(p);
        
        for(int i = 0; i < graph[ind].size(); i++)
        {
            if(graph[ind][i] == -1) continue;
            if(dist[i] == -1) 
            {
                dist[i] = d+graph[ind][i];
                cur.insert(pair<int,int>(dist[i], i));
            }
            else if(dist[i] > d + graph[ind][i])
            {
                cur.erase(pair<int,int>(dist[i], i));
                dist[i] = d+graph[ind][i];
                cur.insert(pair<int,int>(dist[i], i));
            }
        }
    }
}

int main()
{
    int n;
    cin >> n;

    vector<int> price(n);
    for(int i = 0; i < n; i++)
    {
        cin >> price[i];
    }

    graph.resize(n, vector<int>(n, -1));
    dist.resize(n, -1);

    int m;
    cin >> m;
    
    for(int i = 0; i < m; i++)
    {
        int l,r;
        cin >> l >> r;
        l--;
        r--;
        graph[l][r] = price[l];
        graph[r][l] = price[r];
    }

    int s = 0;
    int f = n-1;
    dej(s);
    cout << dist[f];
}