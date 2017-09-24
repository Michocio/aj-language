//unmatchingParamsErr
// Funkcja jako argument, lambda jako parametr aktualny
fun :: [int x, operator :: [int x] -> [int y] ] -> [int z]
{
  returns.z = operator(x) ;
}

// niepoprawna liczba parametrow, lambda
println(fun(7, (int :: [int x, int z]->(x + 7) ) ) );

