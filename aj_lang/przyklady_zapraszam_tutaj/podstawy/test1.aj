class Test
{
  int x;
}

class Dwa
{
  int y;
}

Test a,b;
a.x = 7;
b.x = 10;
Test c;
c.x = 7;
println(a == b);// False
println(a == c);// True
println(c == b);//False
