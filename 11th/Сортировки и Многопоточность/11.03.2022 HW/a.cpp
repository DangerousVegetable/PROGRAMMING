#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;


struct sortnet
{
	int size;
	vector<pair<int,int>> net;

    static vector<pair<int,int>> zigzag(int n) //net having MAX = 1
    {
        vector<pair<int,int>> net;
        int l = 0;
        int r = n-1;
        while(l < r)
        {
            for(int i = l; i < r; i++)
            {
                net.push_back({i,i+1});
            }

            for(int i = r-2; i >= l; i--)
            {
                net.push_back({i,i+1});
            }

            l++;
            r--;
        }

        return net;
    }

	sortnet(int n)
	{
		this->size = n;
		for(int i = 0; i < n-1; i++)
		{
			for(int j = i; j >= 0; j--)
			{
				net.push_back({j,j+1});
			}
		}
	}

    sortnet(int n, vector<pair<int,int>> nt)
    {
        size = n;
        net = nt;
    }
	
	vector<int> sort(vector<int> a)
	{
		for(auto p: net)
		{
			int i = p.first;
			int j = p.second;
			
			if(a[i] > a[j]) swap(a[i], a[j]);
		}
		return a;
	}
	
	void draw()
	{
		vector<vector<char>> matrix(2*size);
		
		for(auto p : net)
		{
			for(int i = 0; i < 2*size; i++)
			{
				if(2*p.first == i || i == 2*p.second)
				{
					matrix[i].push_back('+');
				}
				else if(2*p.first < i && i < 2*p.second)
				{
					matrix[i].push_back('|');
				}
				else if(i % 2 == 0) matrix[i].push_back('-');
				else matrix[i].push_back(' ');
			}
		}
		
		for(auto l:matrix)
		{
			for(char c : l)
			{
				cout << c;
			}
			cout << "\n";
		}
	}
	
	bool check()
	{
		bool b = true;
		vector<int> test(size);
		for(long long l = 1; l < 1 << size; l++)
		{
			bool ok = true;
			long long x = l;
			for(int i = 0; i < size; i++)
			{
				test[i] = x%2;
				x/=2;
			}
			
            #if 0
			for(int i = 0; i < size; i++)
			{
				cout << test[i];
			}
			cout << ": ";
			#endif
			for(auto p: net)
			{
				int i = p.first;
				int j = p.second;
				
				if(test[i] > test[j]) swap(test[i], test[j]);
			}
			
			for(int i = 1; i < size; i++)
			{
				if(test[i-1] > test[i])
				{
					ok = false;
					b = false;
					break;
				}
			}
			
			//printf("%d\n", ok);
		}
		return b;
	}

    int calcmax()
    {
        vector<int> last(size);
        vector<int> layers(net.size());

        for(auto t : net)
        {
            int i = t.first;
            int j = t.second;

            int l = max(last[i], last[j]);
            last[i] = l+1;
            last[j] = l+1;
            layers[l]++;
        }

        int mx = 0;
        for(auto t: layers)
        {
            mx = max(mx, t);
        }

        return mx;
    }
};




int main()
{
	
	vector<int> a = {4,5,1,3,5,7,13,3,0,7,2};
	int n = a.size();
	sortnet sn(n, sortnet::zigzag(n));
	
	sn.draw();

	cout << "Valid: " << (sn.check() ? "+" : "-") << "\n";

    printf("The MAX is: %d\n", sn.calcmax());
	//you can uncomment this line:
	
	a = sn.sort(a);
	for(auto k : a)
	{
		cout << k << " ";
	}
}
