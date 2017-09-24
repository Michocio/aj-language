// Wzajmenie rekurencyjne funkcje
testA :: [int x, fun :: [int x] -> []] -> []
{
   fun(x)
   println(x);
}

testB :: [int x] -> []
{
   if(x > 0)
     testA(x - 1, testB)
}

testA(10, testB)
