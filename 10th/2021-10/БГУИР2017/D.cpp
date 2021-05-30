#include <iostream>
#include <string>
#include <algorithm>
using std::cin;
using std::cout;
using std::string;
using std::getline;
using std::sort;


int main()
{
	int n;
	cin >> n;
	string useless;
	getline(std::cin, useless);

	int* a = new int[n + 1];
	for (int i = 0; i < n; i++)
	{
		string s;
		getline(std::cin,s);
		int k = 0;
		for (int j = 0; j < s.size(); j++)
		{
			if (s[j] == '.') continue;
			k = k * 10 + (s[j] - '0');
		}
		a[i] = k;
	}

	sort(a, a+n);

	int first = a[0];
	int last = a[n - 1];
	int med = (last - first) / 2;

	int min = -1;
	int iq = -1;
	for (int i = 0; i < n; i++)
	{
		int l = a[i] - 1;
		int r = a[i] + 1;
		if (i != 0 && a[i-1] != l)
		{
			if (l - first <= med)
			{
				if (min == -1 || min > l - first)
				{
					min = l - first;
					iq = l;
				}
			}
			else
			{
				if (min == -1 || min > last - l)
				{
					min = last - l;
					iq = l;
				}
			}
		}
		if (i != n - 1 && a[i + 1] != r)
		{
			if (r - first <= med)
			{
				if (min == -1 || min > r - first)
				{
					min = r - first;
					iq = r;
				}
			}
			else
			{
				if (min == -1 || min > last - r)
				{
					min = last - r;
					iq = r;
				}
			}
		}
	}
	if (min == -1)
	{
		if (a[0] != 0) iq = a[0] - 1;
		else iq = a[n - 1] + 1;
	}

	printf("%d.%06d", iq/1000000,iq%1000000);
	return 0;
}