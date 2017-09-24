// Rekurencja + wywolywanie innych funkcji


parzyste :: [] -> []
{
  println("parzyste");
}
nieparzyste :: [] -> []
{
  println("nieparzyste");
}


pisz :: [int n] -> []
{
  println(n);
  if((n % 2) == 0)
    parzyste()
  else
    nieparzyste()

  if(n > 0)
    pisz(n - 1)
}
pisz (10)

