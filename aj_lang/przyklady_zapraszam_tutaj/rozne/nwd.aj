// NWD
int a =125, b = 25;
loop(a != b)
{
  if (a < b)
    b -= a;
  else
    a -= b;
}
print("NWD: ");println(a);//25
