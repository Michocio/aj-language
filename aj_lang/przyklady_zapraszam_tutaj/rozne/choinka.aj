// udoskonalona choinka :)
int n = 10;

int ile = n;
loop(int j = 0; j < ile; j++)
    print(" ");
println("#");
loop ( int i =1; i <= 10; i++)
{
  int ile;
  ile = (n - i);
  loop(int j = 0; j < ile; j++)
    print(" ");
  loop(int j = 0; j < i; j++)
     print("**");
  loop(int j = 0; j < ile; j++)
    print(" ");
  println("");
}
