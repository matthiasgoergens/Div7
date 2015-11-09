Div7
====

Build with

> ghc --make Div7

and run Div7 to get a regular expression that checks if a decimal number is divisible by 7.
Run e.g.

> echo \<number\> | grep -x -E `./Div7`

to see the regular expression in action.

The file regex7 contains the output of the latest run on my machine,
in case you want the regular expression without building and running yourself.

`Griffin' has written a program that produces a shorter regular expression.  You can see their regex on http://codegolf.stackexchange.com/a/3505/32575
