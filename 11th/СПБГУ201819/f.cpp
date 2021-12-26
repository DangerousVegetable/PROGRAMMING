#include <iostream>
#include <vector>
#include <algorithm>
#include <math.h>

using namespace std;


struct dat
{
    int x;
    int f;
    dat(int xx = 0, int ff = 0)
    {
        x = xx;
        f = ff;
    }
};

vector<dat> toprime(int n)
{
    if(n == 1) return vector<dat>(1, dat(1, 0));
    vector<int> d;
    for(int i = 2; i <= sqrt(n); i++)
    {
        int k = 0;
        while(n%i == 0) 
        {
            n /= i;
            k++;
        }

        if(k != 0) d.push_back(k+1);
    }

    if(n != 1) d.push_back(2);
    sort(d.begin(), d.end());

    vector<int> m(d.size());
    m[d.size()-1] = 1;
    for(int i = d.size()-2; i>=0; i--)
    {
        m[i] = m[i+1]*d[i+1];
    }

    int s = 1;
    vector<dat> ans;
    ans.push_back(dat(d[0]*m[0], 0));
    for(int i = 0; i < m.size(); i++)
    {
        for(int j = d[i]-1; j >= 1; j--)
        {
            ans.push_back(dat(j*m[i], s));
            s++;
        }
    }

    return ans;
}

vector<dat> mergedat(vector<dat> v1, vector<dat> v2)
{
    vector<dat> ans;
    int i1 = 0, i2 = 0;
    while(i1 < v1.size() && i2 < v2.size())
    {
        if(v1[i1].x > v2[i2].x) 
        {
            ans.push_back(dat(v1[i1].x, v1[i1].f + v2[i2].f));
            i1++;
        }
        else if(v1[i1].x < v2[i2].x)
        {
            ans.push_back(dat(v2[i2].x, v1[i1].f + v2[i2].f));
            i2++;
        }
        else 
        {
            ans.push_back(dat(v1[i1].x, v1[i1].f + v2[i2].f));
            i1++;
            i2++;
        }
    }
    return ans;
}

int findf(int x, vector<dat> v)
{
    int l = 0;
    int r = v.size()-1;

    if(x >= v[0].x) return 0;

    while(l+1 < r)
    {
        int mid = (l+r)/2;

        if(x >= v[mid].x) r = mid;
        else l = mid;
    }   

    return v[r].f;
}

struct treepoint
{
    vector<dat> val;
    int left;
    int right;

    treepoint(vector<dat> v = vector<dat>(), int l = 0, int r = 0)
    {
        val = v;
        left = l;
        right = r;
    }
};

struct tree
{
    vector<int> el;
    vector<treepoint> tr;

    tree(vector<int> a)
    {
        el = a;
        tr.resize(3*a.size());
        init(0, 0, el.size()-1);
    }

    int calc(int x, int l, int r)
    {
        return getcalc(x, 0, l, r);
    }

private:
    void init(int n, int l, int r)
    {
        if(l == r)
        {
            tr[n] = treepoint(toprime(el[l]), l, r);
        } 
        else
        {
            int mid = (l+r)/2;
            init(2*n+1, l, mid);
            init(2*n+2, mid+1, r);
            tr[n] = treepoint(mergedat(tr[2*n+1].val, tr[2*n+2].val), l, r);
        }
    }

    int getcalc(int x, int n, int l, int r)
    {       
        if(tr[n].left == l && tr[n].right == r)
        {
            return findf(x, tr[n].val);
        }
        else
        {
            int lval = 0;
            int rval = 0;

            int mid = (tr[n].left+tr[n].right)/2;
            if(l <= mid) lval = getcalc(x, 2*n+1, l, min(mid, r));
            if(r >= mid+1) rval = getcalc(x, 2*n+2, max(mid+1, l), r); 

            return lval+rval;
        }
    }
};

int main()
{
    #if 1
    int n;
    cin >> n;
    vector<int> a;

    for(int i = 0; i < n; i ++)
    {
        int k;
        cin >> k;
        a.push_back(k);
    }

    tree t(a);

    int q;
    cin >> q;

    for(int i = 0; i < q; i++)
    {
        int x,l,r;
        cin >> l >> r >> x;
        l--;
        r--;

        cout << t.calc(x, l, r) << "\n";
    }
    #endif

#if 0
    int n,m;
    cin >> n >> m;
    vector<dat> p1 = toprime(n);
    vector<dat> p2 = toprime(m);
    vector<dat> p = mergedat(p1, p2);
    for(int i = 0; i < p.size(); i++)
    {
        cout << p[i].x << " " << p[i].f << "; ";
    }

    int k;
    cin >> k;
    cout << findf(k, p) << "\n";
#endif

#if 0
    vector<int> a = {1,2,3,4,5,6,7};
    tree t(a);

    cout << t.calc(1, 1, 5);

#endif
}