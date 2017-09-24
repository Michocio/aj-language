fun :: [] -> [] {println(1);}

loop (int i = 0; i < 10; i++)
{
   // funkcja lokalna w loop
   fun :: [] -> [] {println(2);}
   fun()//2
}
fun()//1
