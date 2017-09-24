// Niezgodne typy - krzyczy typeChecker
class Testa {
  int x;
}
class Testb {
  bool x;
}

Testa a;
Testb b;
b.x = true;
a.x = b.x;

