// Funkja nie moze sie odwolywac do zewnetrznych zmiennych
fun :: [int x] -> [int y]
{
  x+= 1; // Nie zmienia orginalnego x
  returns.y = x ;
}

int x = 7;
// Wypisuje 8 7
println(fun(x));
println(x);
