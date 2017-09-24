// Podobne do poprzedniego testu, ale z tablica w obiekcie

class Wsp
{
  int x;
  int y;
}
int w = 10;
int h = 20;
class Plansza
{
  Wsp [w][h] pola;
}
Plansza parkiet;

loop(int x = 0; x< 10; x++)
  loop(int y = 0; y< 10; y++)
  {
    Wsp tmp;
    tmp.x = x; tmp.y = y;
    parkiet.pola[x][y] = tmp;
  }

loop(int x = 0; x< 10; x++)
{
  loop(int y = 0; y< 10; y++)
  {
    print(parkiet.pola[x][y]);print(" ");
  }
  println("");
}


