// Przekazywanie obiektow
class Osoba
{
  text imie;
  text nazwisko;
}

show :: [Osoba x] -> []
{
  print(x.imie); print(" "); print(x.nazwisko); println("");
}
Osoba jan;
jan.imie = "Jan";
jan.nazwisko = "Kowalski";
show(jan)
