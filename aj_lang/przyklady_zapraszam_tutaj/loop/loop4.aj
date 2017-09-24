// Deklaracja wykona sie raz
int i = 11;
loop(int i = 0; i < 10; i++)
{
  int n = i;
  println(n);// Zawsze 0 wypisze, ponieważ wykonuje sie raz
}
println(i);// 11
