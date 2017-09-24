
printi::[int co] -> []{
  println(co);

}
printb::[bool co] -> []{
  print(co);
  if (co) {
    println("true");
  }
  else {
    println("false");
  }
}

//----------------------------------------------------------------------
// początek

int a; //deklaracja globalnego a

zwieksz_globalne_a :: [int a, bool m] -> [] {
  printi(a)
  if (m) {
    a++
  }
  else {
    a+=2;
  }
  printi(a)
}
int x = 10;       
zwieksz_globalne_a(x, true)
println(x);

// Nie uda sie, poniewaz typy sie nie zgadzaja
przes_lokal::[bool n] -> []
{
  bool a;
  a = n;
  printb(a)
  zwieksz_globalne_a(a, true)
  printb(a)
}
przes_lokal(true)
