// Niejawne rzutowanie wartości na bool
text prawda = "prawda";
text falsz = "";// pusty jest falszem
int f = 0;
int t = 1;

if(prawda)
{
  println("rzutowanie");
  println(prawda);
}

if(falsz)
{
  println("rzutowanie2");
}
else
  println("puste");

if(f)
  print(f);
if(t)
  println(t);
