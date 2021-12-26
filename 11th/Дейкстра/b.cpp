#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
using namespace std;


vector<vector<int>> graph;
vector<int> dist;
vector<int> pr;

void dej(int s)
{
    set<pair<int,int>> cur; //dist + ind
    cur.insert(pair<int,int>(0, s));
    dist[s] = 0;
    pr[s] = s;
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
                pr[i] = ind;
            }
            else if(dist[i] > d + graph[ind][i])
            {
                cur.erase(pair<int,int>(dist[i], i));
                dist[i] = d+graph[ind][i];
                cur.insert(pair<int,int>(dist[i], i));
                pr[i] = ind;
            }
        }
    }
}

int main()
{
    int n,s,f;
    cin >> n >> s >> f;

    graph.resize(n, vector<int>(n));
    dist.resize(n, -1);
    pr.resize(n, -1);

    for(int i = 0; i < n; i++)
    {
        for(int j = 0; j < n; j++)
        {
            cin >> graph[i][j];
        }
    }

    s--;
    f--;
    dej(s);
    vector<int> ans;
    int ind = f;

    if(dist[f] == -1) cout << -1;
    else
    {
        while(pr[ind] != -1)
        {
            ans.push_back(ind+1);
            if(ind == s) break;
            ind = pr[ind];
        }
        reverse(ans.begin(), ans.end());

        for(int i = 0; i < ans.size(); i++)
        {
            cout << ans[i] << " ";
        }
    }
}