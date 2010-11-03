#!/bin/bash
ghc --make Div7 -ddump-types  -XNoMonomorphismRestriction -O9 -prof -auto-all && grep -x -E `./Div7` test2
