// Funkcje lokalne przeslaniaja
fun :: [] -> [] {println(1);}

test :: [] -> [] 
{
   fun()//1
   fun :: [] -> [] {println(2);}
   fun()//2 --^	
}
test()
