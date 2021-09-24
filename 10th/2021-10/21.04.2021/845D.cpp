#include <iostream>
#include <stack>

using namespace std;


struct PDD
{
    int rule;
    int val;

    PDD(int r = 1, int v = -1)
    {
        this->rule = r;
        this->val = v;
    }
};

PDD pdd[200000];

int main()
{
    int n;
    cin >> n;

    for(int i = 0; i < n; i++)
    {
        int k;
        cin >> k;
        if(k == 1 || k == 3)
        {
            int l;
            cin >> l;
            pdd[i] = PDD(k,l);
        }
        else
        {
            pdd[i] = PDD(k,0);
        }
    }
    
    int v = pdd[0].val;
    stack<int> sp;
    int ov = 0;
    
    int res = 0;
    
    for(int i = 0; i < n; i++)
    {
        if(pdd[i].rule == 1)
        {
            v = pdd[i].val;
            while(sp.size() > 0 && sp.top() < v)
            {
                sp.pop();
                res++;
            }
        }
        if(pdd[i].rule == 2)
        {
            res += ov;
            ov = 0;
        }
        if(pdd[i].rule == 3)
        {
            int restr = pdd[i].val;
            if(v > restr) res++; 
            else
            {
                sp.push(restr);
            }
        }
        if(pdd[i].rule == 4) ov = 0;
        if(pdd[i].rule == 5) sp = stack<int>();
        if(pdd[i].rule == 6) ov++;
        //cout << v << "\n";
    }
    cout << res;
}
