Primer
======
Primer is a simple functional programming language which I wrote for my own amusement. While it's nothing more than my hobby language, a couple of people have found it worth playing with either to learn functional programming or to study a simple language implementation.

Primer is dynamically typed, immutable, uses strict evaluation and has higher order functions and closures.

Primer is open source software and distributed under the MIT license. It builds on Linux, Mac and Windows.

If you have any questions about Primer then please feel free to [email](mailto:philip.armitage@gmail.com) me.

Download and install
--------------------
The latest version of Primer can always be found on [GitHub](http://github.com/parmitage/primer). I will also aim to make binaries available for Linux, MacOS and Windows whenever I think a version is stable enough to play with. The latest of these will be available from the download tab on the above page.

Inside the tarball you'll find the following folders:

src/       - the ML source code for Primer
emacs/     - an Emacs major mode for Primer
examples/  - some example Primer programs
README.md  - this file
LICENSE    - the MIT license

To install a binary release of Primer, unpack the tarball into a directory on your system and set the environment variable `PRIMER_LIBRARY_PATH` to point to this directory. If you're using bash then this should do it:

    export PRIMER_LIBRARY_PATH=/path/to/primer/

On a Windows system, environment variables are set through the control panel.

Hello, World!
-------------
As with most modern languages, Hello World is just a literal string so isn't very interesting to show. The usual equivalent in a functional language is `fac` or `fib` which often rubs people up the wrong way so I'll instead follow the Haskell guys lead and use Quicksort as my Hello World:

    val sort = fun xs ->
       let lt = fun a -> a < head(xs)
           gte = fun a -> a >= head(xs)
       in if head(xs) != [] then
             sort(filter(lt, tail(xs))) ++ [head(xs)]
                             ++ sort(filter(gte, tail(xs)))
          else [] ;

Note that this version of Quicksort is nice for showing off how the language looks but is not tail-recursive so would blow the stack on longer lists. The standard library has a tail recursive version of Quicksort implemented using CPS but it's not as easy to follow.

Quick start
-----------
Primer has a simple REPL which is the quickest way to play with the language. To start the REPL type:

    ./primer

Alternatively, to load an existing Primer source file, simply pass it as a command line argument:

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

Standard Library
----------------
Primer has a modest standard library which can be found in __lib.pri__.

__map__ applies a function to every element in a list.

    map(fun x -> 2 * x, [1,2,3,4,5]);       # [2,4,6,8,10]

__foldl__ and __foldr__ combine successive list elements.

    foldl(fun x y -> x + y, 0, [1,2,3,4]);  # (((0+1)+2)+3)+4 = 10
    foldr(fun x y -> x + y, 0, [1,2,3,4]);  # 1+(2+(3+4)) = 10

__filter__ returns list elements which satisfy a predicate function.

    filter(odd, [1,2,3,2,4,5,6]);           # [1,3,5]

__zip__ combines two lists pairwise to return a new list of 2-tuples.

    zip(l1, [4,5,6]);                       # [[1,4],[2,5],[3,6]]

__reverse__ returns a new list in reverse order.

    reverse("hello");                       # "olleh"

__intersperse__ an atom between list elements.

    intersperse('A', l1);                   # [1,'A',2,'A',3]

__take__ the first n elements of a list.

    take(2, [1,2,3,4,5,6]);                 # [1,2]

__drop__ the first n elements from a list.

    drop(1, [1,2,3,4,5,6]);                 # [2,3,4,5,6]

__takeWhile__ returns items from a list until a predicate returns false.

    takeWhile(even, [2,4,6,1,3,5]);         # [2,4,6]

__dropWhile__ drops items from a list until a predicate returns false.

    dropWhile(odd, [1,3,5,2,4,6]);          # [2,4,6]

__any__ returns true if any item in a list satisfies a predicate.

    any(even, [1,2,3,4,5,6]);               # true

__all__ returns true if all items in a list satisfy a predicate.

    all(odd, [1,2,3,4,5,6]);                # false

__min__ returns the smallest item in a list of orderable items.

    min([7,2,4,5,3,8,6]);                   # 2

__max__ returns the largest item in a list of orderable items.

    max([7,2,4,5,3,8,6]);                   # 8

__sum__ returns the sum of items in a numeric list.

    sum([1,2,3,4,5]);                       # 15

__product__ returns the product of items in a numeric list.

    product([1,2,3,4,5]);                   # 120

__sort__ an orderable list of items.

    sort([4,2,8,1]);                        # [1,2,4,8]

__find__ an element in a list.

    find(3, [1,2,3,4,5]);                   # 3

__findByFun__ find an element in a list with a predicate.

    findByFun('b', key, [[a,1], [b,2]]);    # [b,2]

__replace__ an element in a list.

    replace(2, 'a', [1,2,3,4,5]);           # [1,'a',3,4,5]

__partition__ a list into elements which do and don't satisfy a predicate.

    partition(even, [1,2,3,4,5,6]);         # [[6,4,2],[5,3,1]]

__group__ a list into a list of lists of equal adjacent elements.

    group([1,1,2,3,4,4,5,6,6,6]);           # [[1,1],[2],[3],[4,4],[5],[6,6,6]]

__mapPair__

__collect__


Example programs
----------------
The tarball includes a few sample programs to highlight the features of the language. These include...

Editing
-------
You can of course edit Primer code in any text editor but you will find the beginnings of an Emacs major mode in the source distribution.

History
-------
The initial version of Primer was written in C, mainly because I was familiar with Lex and Yacc. This was a very poor reason for choosing an implementation language and I later decided to rewrite the interpreter in something nicer. My choices were between Scheme and an ML family language. I decided to go with ML because at the time Scheme was going through some standardisation pains and it wasn't clear what implementations were going to do. Specifically, I chose OCaml because it seemed to be the most practical dialect of ML. However, I'm considering porting everything to SML (see point 0 in the Future section below). If I do this then hopefully it will be the last time I feel the urge to port it.

Future
------
As Primer is my hobby project, I suspect it will never be truly finished because I want to keep fiddling with it.. My current todo list is below although I should stress that I may decide not to do some of these things and I may tackle them in a different order.

0. Re-write in SML. The first version of Primer was written in C for no good reason. The current version is written in OCaml. I'd like to investigate the possibility of rewriting it in SML. I like the orderliness of SML compared to OCaml (it reminds me of Scheme compared to Common Lisp in many ways) but I need to see how practical it is to use for binary distribution of Primer across the three platforms I want to support.

1. Add the match statement from the original C implementation of Primer.

2. Add proper library loading from the original C implementation of Primer.

3. A compiler to JavaScript. It seems that everything should target the browser these days so the first compilation target for Primer should be JavaScript.

4. A compiler to C. Native code compilation would also be nice and C can act as a portable assembler. Another option could be to target LLVM byte-code.