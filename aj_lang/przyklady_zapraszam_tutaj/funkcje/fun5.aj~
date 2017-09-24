// Zwracanie kilku wartosc
fun :: [int x] -> [int y, int z]
{
  returns.y = x ;
  x += 1; // Nie zmienia orginalnego x
  returns.z = x ;
}

// 1 0
println(fun(0));

Fun zwrot = fun(0);
// 1 0
println(zwrot);
// 0
println(zwrot.y);
// 1
println(zwrot.z);
