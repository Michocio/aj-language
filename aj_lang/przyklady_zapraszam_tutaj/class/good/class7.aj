// Przypisanie calych obiektow
class Testa {
  int x;
}

Testa a,b;
a.x = 7;

// Wartosc deafultowa == 0
println(b.x);
b = a;
// Wartosc po przypisaniu obiektow == 7
println(b.x);
println(a.x);
a.x=8;
println(b.x);//ciagle 7
println(a.x);//juz 8

