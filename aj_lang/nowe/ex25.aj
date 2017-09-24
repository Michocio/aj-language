// Funkcja jako argument, lambda jako parametr aktualny
fun :: [test :: [int x] -> [text y] ] -> [text z]
{
  returns.z = test(0) ;
}

// niepoprawny typ zwracany przez lambde
println(fun((text :: [int x]->("witam"))));

