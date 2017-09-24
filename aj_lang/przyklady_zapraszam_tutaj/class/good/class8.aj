class Test
{
  text b;

}
fun :: [Test a] -> [text b]
{
   returns.b = a.b;
}
Test a;
a.b="test";
println(fun(a));//test
