int [10][10] plansza;

loop(int x = 0; x< 10; x++)
  loop(int y = 0; y< 10; y++)
    plansza[x][y] = x;

loop(int x = 0; x< 10; x++)
{
  loop(int y = 0; y< 10; y++)
  {
    print(plansza[x][y]); print(" ");
  }
  println("");
}

class Wsp
{
  int x;
  int y;
}

int w = 10;
int h = 20;
Wsp [w][h] pola;

loop(int x = 0; x< 10; x++)
  loop(int y = 0; y< 10; y++)
  {
    pola[x][y].x = x;
    pola[x][y].y = y;
  }

loop(int x = 0; x< 10; x++)
{
  loop(int y = 0; y< 10; y++)
  {
    print("(");print(pola[x][y].x); print(", ");print(pola[x][y].y);print(")");print(" ");
  }
  println("");
}






