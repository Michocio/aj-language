// Dziala przeslanianie w odpowiedni sposób
int x = 0;
println(x);//0

if(x==0)
{
  x = 7;
  println(x);//7
  int x = 9;
  println(x);//9
  x = 12;// nowe x
  println(x);//12
}
println(x);//stare x = 7

switch(x)
{
   case 0:
   {
      println("a");
   }
   case 7:
   {
      x = 9;
      int x = 10;
      println(x);//10
   }
}
println(x);//9
