fun :: [] {} // parser error
fun :: [int x] -> [int x] {} //ok
fun :: [int x, int x] -> [int x] {} // error typechecker, ta sama nazwa

