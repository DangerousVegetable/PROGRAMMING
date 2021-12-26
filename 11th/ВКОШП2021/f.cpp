#include <iostream>
#include <vector>
#include <map>
#include <math.h>
using namespace std;

vector<int> start;
vector<int> vel;

int gcd(int n, int m)
{
    if(n == 0) return m;
    if(m == 0) return n;
    if(n < m) return gcd(n, m%n);
    else return gcd(n%m, m);
}

struct ratio
{
    int n;
    int m;
    ratio(int x = 0, int y = 1)
    {
        n = x;
        m = y;
        this->simplify();
    }

    void simplify()
    {
        int d = gcd(abs(n),abs(m));
        n = n/d;
        m = m/d;
        if(m < 0) 
        {
            m = -m;
            n = -n;
        }
    }
};

ratio operator+ (const ratio& r1, const ratio& r2)
{
    ratio r(r1.n*r2.m + r1.m*r2.n, r1.m*r2.m);
    r.simplify();
    return r;
}

ratio operator- (const ratio& r1, const ratio& r2)
{
    ratio r(r1.n*r2.m - r1.m*r2.n, r1.m*r2.m);
    r.simplify();
    return r;
}

bool operator< (const ratio& r1, const ratio& r2)
{
    if(r1.n == r2.n)
    {
        return r1.m < r2.m;
    }
    else return r1.n < r2.n;
}

struct ans
{
    int num; //-1 - inf, 0 - zero solutions, 1 - one solution
    ratio r;

    ans(int n = 0, ratio q = ratio())
    {
        num = n;
        r = q;
    }
};

bool operator< (const ans& a1, const ans& a2)
{
    if(a1.num == a2.num)
    {
        return a1.r < a2.r;
    }
    else return a1.num < a2.num;
}

ans solve(int a, int b)
{
    if(a == 0)
    {
        if(b == 0) return ans(-1);
        else return ans(0);
    }
    else return ans(1, ratio(-b, a));
}

int main()
{
#if 1
    int n;
    cin >> n;

    start.resize(n);
    vel.resize(n);
    for(int i = 0; i < n; i++)
    {
        int x,v;
        cin >> x >> v;
        start[i] = x;
        vel[i] = v;
    }
#endif
#if 0
    ratio r1(14,5);
    ratio r2(14,3);

    ratio r3 = r1-r2;
    cout << r3.n << " " << r3.m; 
#endif

    vector<vector<map<ans,int>>> sol(n, vector<map<ans,int>>(n));
    for(int i = 0; i < n; i++)
    {
        for(int j = i+1; j < n; j++)
        {
            for(int k = 0; k < n; k++)
            {
                if (k == i || k == j) sol[i][j][ans(-1)]++;
                else
                {
                    //int j-i = j-i;
                    //int k-j = k-j;
                    //int vel[k] = vel[k];
                    //int vel[j] = vel[j];
                    //int vel[i] = vel[i];
                    //int start[k] = start[k];
                    //int start[j] = start[j];
                    //int start[i] = start[i];
                    int a = (j-i)*(vel[k]-vel[j]) - (k-j)*(vel[j]-vel[i]);
                    int b = (j-i)*(start[k]-start[j]) - (k-j)*(start[j]-start[i]);
                    ans an = solve(a,b);
                    if(an.num == 1 && an.r.n < 0) continue;
                    sol[i][j][an]++;
                }
            }
        }
    }

    int maxans = 0;
    for(int i = 0; i < n; i++)
    {
        for(int j = i+1; j < n; j++)
        {
            int cur = sol[i][j][ans(-1)];
            
            //cout << i << j << " : " << cur << '\n';
            int maxq = 0;
            for(auto x: sol[i][j])
            {
                //cout << i << " " << j << ": " <<  x.first.num << " " << x.first.r.n << " " << x.first.r.m << " - " << x.second << "\n";
                if(x.first.num == 1)
                {
                    maxq = max(maxq, x.second);
                }
            }
            cur += maxq;

            maxans = max(cur, maxans);
        }
    }
    cout << maxans;
}