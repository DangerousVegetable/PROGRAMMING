#include <iostream>
#include <list>
 
#define MOD 1000000007 
 
using namespace std;
 
long long int lengths[200010];
 
int clever(int l, int m)
{
    int k = l;
    long long int r = 0;
    
    while (k >= 10)
    {
        int x = k%10;
        k = k/10;
        r=(r+lengths[x+m])%MOD;
    }
    r=(r+lengths[k+m])%MOD;
    
    return r;
}
 
int num[200000];
int ms[200000];
 
int main()
{
    long long int l[10] = {1,0,0,0,0,0,0,0,0,0};
    lengths[0] = 1;
    for(int i = 1; i < 200010; i++)
    {
        long long int nine = l[9];
        l[9] = 0;
        for(int i = 8; i >= 0; i--)
        {
            long long int k = l[i];
            l[i] = 0;
            l[i+1] = (l[i+1] + k)%MOD;
        }
        l[0] = (l[0] + nine)%MOD;
        l[1] = (l[1] + nine)%MOD;
        /*
        for(int i = 0; i < 10; i++)
        {
            cout << l[i];
        }
        cout << "\n";*/
        
        lengths[i] = (lengths[i-1]+nine)%MOD;
    }
    
    //cout << lengths[11];
    
    int t;
    cin >> t;
    for(int i = 0; i < t; i++)
    {
        cin >> num[i];
        cin >> ms[i];
    }
    
    for(int i = 0; i < t; i++)
    {
        cout << clever(num[i], ms[i]) << "\n";
    }
}
