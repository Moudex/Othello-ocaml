Othello-ocaml
=============

This is an othello game writted in ocaml language.
There are a various possible mode : human vs human, human vs IA and IA vs IA
You can custom each IA mode :
* Algorithm (Random, MinMax, AlphaBeta, ...)
* Level of recursivity
* Eval function

Build and install
-----------------
`$ make && make clean`
Lanch the game
`$ ./othello`

You can copy to your games folder :
`# cp ./othello /usr/local/games`
You must be superuser.
And now you can lanch it everywere.

BUGS
-----
* End of game when there are only one color

TODO
-----
* Better eval function
* End menu
* Console mode
* Random sur MinMax et AlphaBeta sur score identique
