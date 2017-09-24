<h1> *Aj* language interpreter </h1>

*Aj* is imperative programming language created by me during bechelor CS studies.  It consists of several elements from functional and object-oriented languages.
I tried to choose my favourite things from few languages and than mix them in my creation. What's more I rejected some frustrating constructions and that's how *aj* was created. It takes some syntax from *c++* and *java*, but at the same time function defining looks familiar to *haskell*.
Repositorium files include:
* calc.cf - grammar in bnfc form
* Abscalc.hs - parsing tree's data structers
* ErrM.hs - error handling
* Error.hs - own implementation of error handling
* Lexcalc.hs - *lexer* internals
* Parcalc.hs - parsing to LR tree
* Skelcalc.hs - just skeletons of functions used by interpreter
* Core.hs - main and commonly used interpreter's structers
* Declarations.hs - funtions connected with definig things in aj
* program.hs - kind of *main* file. Connects few modules
* Storable.hs - everything what is connected with values that can be stored (int, string etc...).
* Interpreter.hs - expressions + statements
* TypeChekcer.hs - module does static code analisis

Implemented features:
* variables, assingments
* if [ else and elseif], switch
* loop - only one loop construction, that can easily act like while and for
* arithmetic
* comparing
* functions (multiple parametrs to return] and procedures
* recursion
* IO operation: print
* variable overriding and vars scopes
* static typic
* handling run time errors [ i.e. dividing by 0, out of range array]
* records - simple classess
* arrays / lists [multidimensional]
* passing functions as functions arguments (even lambda functions)
* anonymous functions
* ested functions with static binding

<h2> Features </h2>

<h3> Typy </h3>

* int - range like haskell integer
* true / false values
* text - one type for char / string
* double - real numbers
* own types ...
* arrays


Implicit conversions between multiple types are implemented (where this makes sense). For example, the empty text goes to false,
The text compared to the number reflects as its length.

<h3> Conditionals </h3>
<h4> if </h4>
Possible variants with multiple elseifs, with else, without else, etc.if one line statemntthen without brackets.
Conditions are impinging implicitly on bool
<h5> switch </h5>
Traditional semantics of the switch, except that instead of default is else, after the switch. Any possible object that can be compared, can appear
as a switch argument (not only numbers as in other languages)
 

<h3> Loop </h3>
Loop - one loop instead while and for. Semantics is more like for, but you can cause it to work like a while :).
We can define variables in the loop header, possible multiple chapters (use |). The semantics of variables are more interesting, because those
declared in the middle of the loop are created only in first step. As a condition may be passed any variable, not necessarily declared in the loop.


<h3> Functions </h3>
We can pass both function and normal variables to functions. Passing only by value is possible - so that it was not
side effects. We can return many values but not functions. There are no static binding in functions. Every function declaration generates a type with the same name - it can be assigned values ​​returned by the function. If we return one value -
Simple return, without having to assign to a function type.

<h3> Data Structures </h3>

<h4> arrays </h4>
Any number of dimensions and any type of arrays, an array can also occur in records.

<h4> records </h4>
Only variable fields are possible, you cannot add functions to classes at this time. Possible to pass objects to functions,
creating arrays from them, or compare objects of the same type.


<h3> IO </h3>

<h4> print </h4>
Allows you to print almost any type, listing your own type (object) prints a list of values assigned to the object.

<h1> Notes </h1>
My grammar is long - it results from a lot of syntactic sugar.
Assignment prescribes value, not pointer, but the construction of the environment appropriately allows you to assign indicators as well.

**Checkout examples dirs**

<h1> Compiling </h1>
Just type make in main folder














