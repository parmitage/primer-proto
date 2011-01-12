Primer
======
Primer is a very simple functional programming language written for fun. The OCaml [source](http://github.com/parmitage/primer) as well as binaries for Linux, MacOS X and Windows are available. If you have any comments about Primer then please [email](mailto:philip.armitage@gmail.com) me.

To start the REPL, change to the directory where you unpacked the tarball and type:

    ./primer

To load an existing Primer source file pass it as a command line argument:

    ./primer MyFile.pri

Bindings are immutable.

    val pi = 3.14159;

The body of a function is a single expression, the value of which is the return value.

    val areaOfCircle = fn (r) pi * r * r;

Functions are first class objects, can be passed anonymously and are higher order.

    val simpleOpSquared = fn (f, x) f(x) * f(x);
    simpleOpSquared(fn (x) x / 3, 12)       # 16

Primer supports closures.

    val makeAdder = fn (y) fn (a) y + a;
    val add2 = MakeAdder(2);
    add2(2);                                # 4

__let__ introduces local definitions.

    let x = 12 in x + 4;

Lists can be nested and are heterogeneous.

    val xs = [4, [5.32, [pi], [], true], AreaOfCircle, 'a', "aaa"];

Several functions provide access to list elements.
 
    Head(xs);
    Tail(xs);
    Last(xs1);
    Length(xs);

A list can by accessed by index with the __at__ operator.

    xs at 4;

An item can be prepended to a list with the __cons__ operator.

    1 :: [2,3,4];

To concatenate two lists use the __append__ operator.

    [1,2,3] ++ [4,5,6];

Strings are just lists of characters.

    Head("hello");
    "hello" ! 3;

The type of a value can be tested with the __is__ operator.

    123.45 is string;                       # false
    pi is float;                            # true

The __as__ operator converts between types.

    123.45 as string;                       # "123.45"
    "123.45" as float;                      # 123.45

The __if__ statement is an expression so the __else__ branch is mandatory.

    val count = fn (xs)
       if xs != []
       then 1 + count(Tail(xs))
       else 0;

Tail-recursive functions are optimised as in this accumulator version of Count.

    val count = fn (xs)
       let counter = fn (a, xs)
          if xs != []
          then counter(a + 1, Tail(xs))
          else a
       in counter(0, xs);

Primer has a modest standard library which can be found in __Library.pri__.

__Map__ applies a function to every element in a list and returns a new list containing the transformed values.

    Map(fn (x) 2 * x, [1,2,3,4,5]);         # [2,4,6,8,10]

__FoldL__ and __FoldR__ uses a two parameter function to combine successive list elements.

    val add = fn (x, y) x + y;
    FoldL(add, 0, [1,2,3,4,5]);             # ((((0+1)+2)+3)+4)+5 = 15
    FoldR(add, 0, [1,2,3,4,5]);             # 1+(2+(3+(4+(5+0)))) = 15

__Filter__ applies a predicate function to each element of a list returning those elements for which the function returns true.

    Filter(Odd, [1,2,3,2,4,5,6]);           # [1,3,5]

__Zip__ takes two lists and combines them pairwise to return a new list of 2-tuples.

    Zip(l1, [4,5,6]);                       # [[1,4],[2,5],[3,6]]

__Reverse__ returns a new list in reverse order.

    Reverse("hello");                       # "olleh"

__Intersperse__ takes an atom and a list and returns a new list with the atom interspersed between the original list elements.

    Intersperse('A', l1);                   # [1,'A',2,'A',3]

__Take__ returns the first n items from a list.

    Take(2, l1);                            # [1,2]

__Drop__ returns a list without its first n items.

    Drop(1, l1);                            # [2,3]

__TakeWhile__ evaluates each item in a list in turn using a supplied function and returns the items from the list until the function returns false.

    TakeWhile(Even, l1);                    # []

__DropWhile__ evaluates each item in a list in turn using a supplied function and drops the items from the list until the function returns false.

    DropWhile(Odd, l1);                     # [2,3]

__Any__ returns true if any items in a list passes the supplied predicate function otherwise it returns false.

    Any(Even, [1,2,3,4]);                   # true

__All__ returns true if all items in a list passes the supplied predicate function otherwise it returns false.

    All(Odd, [3,4,5,6]);                    # false

__Min__ returns the smallest item in a list. It requires that the list contains items which can be compared.

    Min([7,2,4,5,3,8,6]);                   # 2

__Max__ returns the largest item in a list. It requires that the list contains items which can be compared.

    Max([7,2,4,5,3,8,6]);                   # 8

__Sum__ returns the sum of the items in the list. It requires that the items in the list are numeric.

    Sum([1,2,3,4,5]);                       # 15

__Product__ returns the product of the items in the list. It requires that the items in the list are numeric.

    Product([1,2,3,4,5]);                   # 120

__Sort__ is an implementation of the Quicksort algorithm.

    Sort([4,2,8,1]);                        # [1,2,4,8]