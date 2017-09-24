// Zwracanie obiektow
// Przekazywanie obiektow
class Osoba
{
  text imie;
  text nazwisko;
}

show :: [Osoba x] -> [Osoba y]
{
  print(x.imie); print(" "); print(x.nazwisko); println("");
  Osoba tmp = x;
  tmp.imie = "Lukasz";
  tmp.nazwisko = "Piszczek";
  returns.y = tmp;
}
Osoba jan;
jan.imie = "Jan";
jan.nazwisko = "Kowalski";
show(jan)
println(show(jan));
Osoba nowa = show(jan);
println(nowa);
