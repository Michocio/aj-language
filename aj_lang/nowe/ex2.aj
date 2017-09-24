// Demonstracja semantyki loop
int x = 7;

loop(int i = 0; i < 10; i++)
{
  x = 10;
  int x = 11;
  // semantyka loop, usuwa po pierwszym obiegu wszystkie definicje
  println(x);//11, za pierwszym razem, potem 10
}

println(x);//10
