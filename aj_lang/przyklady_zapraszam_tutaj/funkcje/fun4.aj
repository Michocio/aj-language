int x;

// Funkja nie moze sie odwolywac do zewnetrznych zmiennych
fun :: [int z] -> [int y]
{
  x += 1; // Error!
  returns.y = x ;
}

fun(0)
