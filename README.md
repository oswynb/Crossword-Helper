Crossword-Helper
================

Small Haskell program to assist with typesetting of crosswords

Usage:

`crossword-helper $target $output`
This will output two files: `$output.tex` and `$output-solution.text`
The program should automatically open the tex files before terminating.

Install with

`stack install`

Or build and run locally with

```
stack build
stack exec crossword-helper Crosswords/008.cross Crosswords/test
```
