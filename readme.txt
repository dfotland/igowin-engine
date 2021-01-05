
For 2007, there are two new parameters to compmove(), that specify moves to skip.  
This is used by the hint function.  After the first search, you can do another search,
skipping the forst move, to get the second best move.

There are two new source files learn.c and learn.h that must be included in the project.

The old learning code is no longer included (fusnode.h, g2fuseki.c g2fuslrn.h)

The new search has been improved.

Add code as shown in sample.c so call learn_add_game after each game, and it will learn
patterns from each game and it will not repeat a lost game.

The Go algorithm consists of all the files that start with "g2".
joseki.c is the joseki library.

Ui2g2.h defines the API for teh go engine.

patterns.c is the pattern database
learn.c is the open pattern learning code.
znum.c is a file of random numbers for a hashing function

The file "g2check" has code that is only used for debugging.  It
should not be linked with the production version.

The file "gepat.h" has #defines that must be set correctly for the
word size of the machine you are compiling for.  UINT8, UINT16, and
UINT32 must be set to the correct unsigned types for 8, 16, and 32 bit
integers.

The file "UI2G2.H" has the interface from your user interface to the
Go engine.

sample.c is sample code to show how use the go engine.

The following #defines are used in the program:

For the production version:

When building a debugging version, define:

G2DEBUGOUTPUT - for debug commands
CHECK - includes range checking and data structure checking code
TEST - includes test commands

 
DATA FILES:




