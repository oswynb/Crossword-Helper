Crossword-Helper
================

Small Haskell program to assist with typesetting of crosswords

Only tested in Windows, don't blame me, Ubuntu overheats my laptop! I'll test on linux soon. Likewise I haven't tested the Makefile but it should work.

Compile in Windows with

ghc Main.hs Latex.hs Rep.hs -o CrosswordHelper -O2 -Wall

Format to run is

CrosswordHelper.exe (input .cross file) (output prefix)

or ./CrosswordHelper ... in Linux/Mac

e.g.

CrosswordHelper.exe Crosswords/008.cross Crosswords/thisismyoutputname

will output two files: Crosswords/thisismyoutputname.tex and Crosswords/thisismyoutputname-solution.tex

If you have latex installed, the program should automatically open the tex files before terminating.