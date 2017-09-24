// Wypisuje liczby pierwsze
int n = 100;
bool[n + 1] tab;
int w = 0;

loop(int i = 2; i <= n; i++) 
  tab[i] = true;
int g = sqrt(n);
loop(int i = 2; i <= g; i++)
{
   if(tab[i])
   {
     w = i * i;
     loop(w <= n)
     {
        tab[w] = false; 
        w += i;
     }
   }
   
}
loop(int i = 2; i <= n; i++) if(tab[i]) { print(i); print(", ");}
println("");

