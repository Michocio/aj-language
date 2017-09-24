// Przepisywanie tablic
int [10][10] a;
int [10][10] b;

a[5][5] = 7;
println(b[5][5]);//0
b = a;
println(b[5][5]);//7
a[5][5] = 9;
println(b[5][5]);//7
println(a[5][5]);//9

