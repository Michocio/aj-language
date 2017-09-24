//lambdaErr
// Funkcja jako argument, lambda jako parametr aktualny
fun :: [int x, operator :: [int x] -> [int y] ] -> [int z]
{
  returns.z = operator(x) ;
}

// niepoprawny typ zwracany przez lambde
println(fun(7, (int :: [int x]->(true) ) ) );

