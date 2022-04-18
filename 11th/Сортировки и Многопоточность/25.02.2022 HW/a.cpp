#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;


struct sortnet
{
	int size;
	vector<pair<int,int>> net;
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
			
			for(int i = 0; i < size; i++)
			{
				cout << test[i];
			}
			cout << ": ";
			
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
			
			printf("%d\n", ok);
		}
		return b;
	}
};




int main()
{
	
	vector<int> a = {4,5,1,3,5,7,13,3,0,7};
	int n = a.size();
	sortnet sn(n);
	
	sn.draw();

	//you can uncomment this line:
	//cout << sn.check() << "\n";
	
	a = sn.sort(a);
	for(auto k : a)
	{
		cout << k << " ";
	}
}
