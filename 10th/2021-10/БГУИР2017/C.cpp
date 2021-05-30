#include <iostream>
#include <string>
#include <map>
#include <stack>
using std::cout;
using std::cin;
using std::string;
using std::map;
using std::stack;

map<char,long long> min;
map<char,long long> max;

map<char,long long> coef;

int main()
{
    int n, m;
    cin >> n >> m;

    string s;
    cin >> s;

    for(int i = 0; i < m; i++)
    {
        char a;
        long long mi,ma;
        cin >> a >> mi >> ma;
        min[a] = mi;
        max[a] = ma;
    }

    stack<bool> signs;

    signs.push(true);
    signs.push(true);

    bool op = true;


    long long sum = 0;
    for(int i = 0; i < n; i++)
    {
        /*if(s[i] == '+'|| s[i] == '-')
        {
            if(s[i+1] == '(') 
            {
                signs.push(s[i] == '+' ? signs.top(): !signs.top());
            }
            else
            {

            }
        }*/
        
        /*if(signs.size() == 0) {cout << "HERE: " << i;};
        cout << signs.size() << " ";*/

        if(s[i] == '+' || s[i] == '-')
        {
            if(s[i] == '+') signs.push(signs.top());
            if(s[i] == '-') signs.push(!signs.top());
            op = true;
        }
        else
        {
            if(s[i] == '(') {if(!op) signs.push(signs.top());} 
            else if(s[i] == ')') signs.pop();
            else
            {
                if(s[i] >= '0' && s[i] <= '9')
                {
                    sum+=(s[i]-'0')*(signs.top() ? 1:-1);
                }
                else coef[s[i]] += signs.top() ? 1:-1; 

                if(op) signs.pop();
            }

            op = false;
        }
        //signs.top();
        //cout << signs.size() << '\n';
    }


    for(auto x: coef)
    {
        if(x.second < 0) sum+=x.second*min[x.first];
        else sum+=x.second*max[x.first];
    }

    cout << sum;

}