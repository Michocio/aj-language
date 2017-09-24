// Funkcje lokalne przeslaniaja
fun :: [] -> [] {println(1);}

test :: [int x] -> [] 
{
   fun()//za pierwszym razem 1, potem 2
   fun :: [] -> [] {println(2);}
   fun()//zawsze 2
   if(x > 0)
      test(x - 1)
}
test(10)
