#include <iostream>
#include <string>
#include <set>


using namespace std;

set <long long int> sq;

long long int reverse(long long int x)
{
	long long int ans = x % 10;
	x = x / 10;
	while (x != 0)
	{
		ans = ans * 10 + (x % 10);
		x = x / 10;
	}
	return ans;
}

int main() {
	long long int w = 100000000000;
	for (long long int i = 1; i * i <= w; i++)
	{
		sq.insert(i*i);
	}
	long long int a;
	long long int b;
	cin >> a >> b;
	auto l = sq.lower_bound(a);
	auto r = sq.upper_bound(b);
	long long int c = 0;
	while (l != r)
	{
		if ((*l >= a) && (*l <= b))
		{
			if (sq.find(reverse(*l)) != sq.end())
			{
				if ((*l) % 10 != 0)
				{
					c++;
				}
			}
			l++;
		}
		else
		{
			break;
		}
	}
	cout << c;
	return 0;
}