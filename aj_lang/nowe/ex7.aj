fun :: [] -> [] {println(1);}
int x = 0;
if(not x)
{
  fun :: [] -> [] {println(2);}//wchodzi
  fun()//2
}
fun()//1 - funkcja z samej góry
