// Wiązanie funkcji w rekurencji
test :: [int x] -> []
{
  println("b");
  test :: [int x] -> []
  {
     println("a");
     if(x > 0 )
        test(x - 1)
  }
  if(x > 0 )
    test(x - 1)
}

test(10)//"b", a potem "a"
