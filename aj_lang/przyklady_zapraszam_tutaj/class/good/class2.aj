// Obiekt w obiekcie
class Test
{
  int x;
}
class Big 
{
 int y;
 Test a;
}

Big z ;
Test a;
a.x = 77;
z.a = a;
println(z);//Trans obj ["77","0"]
