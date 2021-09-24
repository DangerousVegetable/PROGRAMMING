#include <iostream>

struct point
{
    int x;
    int y;
    point(int x = 0, int y = 0)
    {
        this->x = x;
        this->y = y;
    }
};

int operator*(point p1, point p2)
{
    return p1.x*p2.x + p1.y*p2.y;
}

int operator^(point p1, point p2)
{
    return p1.x*p2.y - p1.y*p2.x;
}

point operator+ (const point& p1, const point& p2)
{
    return point(p1.x+p2.x, p1.y+p2.y);
}

point operator- (const point& p1, const point& p2)
{
    return point(p1.x-p2.x, p1.y-p2.y);
}

int len2(point p)
{
    return (p.x*p.x + p.y*p.y);
}

double dist2(point p1, point p2)
{
    return len2(p2-p1);
}

int side(point p1, point p2, point p)
{
    return (p2-p1)^(p-p1);
}

int signside(point p1, point p2, point p)
{
    int res = side(p1,p2,p);
    if(res > 0) return 1; 
    if(res == 0) return 0;
    else  return -1;
}

/*bool ondifferentsides(point p1, point p2, point p3, point p4)
{
    int res1 = signside(p1,p2,p3);
    int res2 = signside(p1,p2,p4);
    
}*/
bool insegment(point p1, point p2, point p)
{
    int res = signside(p1,p2,p);
    if(res == 0)
    {
        return (dist2(p,p1)+dist2(p,p2) <= dist2(p1,p2));
    }
    else return false;
}

bool intersect(point p1, point p2, point p3, point p4)
{
    if(insegment(p1,p2,p3) || insegment(p1,p2,p4) || insegment(p3,p4,p1) || insegment(p3,p4,p2))
    {
        //std::cout << "here";
        return true;
    }
    else
    {
        int r3 = signside(p1,p2,p3);
        int r4 = signside(p1,p2,p4);
        int r1 = signside(p3,p4,p1);
        int r2 = signside(p3,p4,p2);

        if(r1*r2 < 0 && r3*r4 <0) return true;
        else return false; 
    }
}

int main()
{
    int n;
    std::cin >> n;
    point* m = new point[n];

    for(int i = 0; i < n; i++)
    {
        int x,y;
        std::cin >> x >> y;
        m[i] = point(x,y);
    }

    if(n == 2) 
    {
        std::cout << -1;
        return 0;
    }
    else
    {
        for(int i = 0; i < n; i++)
        {
            int j = (i+(n-1))%n;
            int k = (i+1)%n;

            if(((m[i]-m[j])^(m[k]-m[i])) == 0) //3 точки подряд на одной прямой
            {
                std::cout << "3 subsequent points are on a line " << -1;
                return 0;
            }
        }
        int d = (m[1]-m[0])^(m[2] - m[1]);
        if(d > 0) d = 1;
        else d = -1;

        //std::cout << d << '\n';

        for(int i = 0; i < n; i++)
        {
            int j = (i+(n-1))%n;
            int k = (i+1)%n;

            if(((m[i]-m[j])^(m[k]-m[i]))*d < 0) //поворот в другую сторону
            {
                std::cout << "not a convex polygon " << 0;
                return 0;
            }
        }
        for(int i = 0; i < n; i++)
        {
            for(int j = i+2; j < n; j++)
            {
                if((j-i)%n == 1 || (j-i)%n == n-1) continue;
                else
                {
                    point p1 = m[i];
                    point p2 = m[(i+1)%n];
                    point p3 = m[j];
                    point p4 = m[(j+1)%n]; 
                    if(intersect(p1,p2,p3,p4)) 
                    {
                        std::cout << i << j << "self-intersection found " << 0;
                        return 0;
                    } 
                }
            }
        }
        std::cout << 1;
        return 0;
    }
}