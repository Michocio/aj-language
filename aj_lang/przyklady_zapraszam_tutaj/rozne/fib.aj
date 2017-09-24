fib::[int n] -> [int x]
{
  //println(n);
  if(n<3)
  {
    returns.x  = 1;
  }
  else
  {
    returns.x = fib(n - 2) + fib(n - 1);
  }
}
println(fib(19));
