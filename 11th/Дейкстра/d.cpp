#include <iostream>
#include <vector>
#include <set>
using namespace std;


vector<vector<int>> graph;
vector<int> dist;
vector<vector<int>> weight;

void dej(int s, int w)
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
            if(graph[ind][i] == -1 || weight[ind][i] < w) continue;
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

    if(n == 1) 
    {
        cout << 10000000;
        return 0;
    }

    graph.resize(n, vector<int>(n, -1));
    dist.resize(n, -1);
    weight.resize(n, vector<int>(n));
    int m;
    cin >> m;
    
    long long maxw = 0;
    for(int i = 0; i < m; i++)
    {
        int l,r,t;
        long long w;
        cin >> l >> r >> t >> w;
        l--;
        r--;
        graph[l][r] = t;
        graph[r][l] = t;
        weight[l][r] = w;
        weight[r][l] = w;
        maxw = max(maxw, (w-3000000)/100);
    }

    int s = 0;
    int f = n-1;

    long long w = 3000000;
    long long left = 0;
    long long right = maxw+2;

    while(left+1 < right)
    {
        dist = vector<int>(n, -1);
        long long mid = (left+right)/2;
        dej(s, w+100*mid);

        if(dist[f] != -1 && dist[f] <= 1440) left = mid;
        else right = mid;
    }
    cout << left;
}