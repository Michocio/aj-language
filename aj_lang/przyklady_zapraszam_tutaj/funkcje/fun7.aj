// Funkcja jako argument, lambda jako parametr aktualny
fun :: [int x, operator :: [int x] -> [int y] ] -> [int z]
{
  returns.z = operator(x) ;
}

// dodawanie = 14
println(fun(7, (int :: [int x]->(x + 7) ) ) );

// odejmowanie = 0
println(fun(7, (int :: [int x]->(x - 7) ) ) );
