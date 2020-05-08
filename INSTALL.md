# Installation

1.  After opening the zip file, type ``make build`` in the terminal to build our system.
2.  Now type ``make build`` in the terminal: this should start our game by loading an empty scrabble board.

# Gameplay

1. To play our game, you will be prompted after running ``make run`` to type
in the number of players.

2. After that, each player will be prompted ``move | swap | pass > `` where
they can either execute a move by placing letters on the board, swapping
tiles, or passing their turn

3. To execute a move on the Scrabble board, you will need to give the following
inputs ``move [row] [column] [direction] [word] ``  
For example, ``move 2 3 a orange``

* The ``row`` input ranges from __0__  to __14__.
* The ``column`` input ranges from __0__ to __14__.
* The ``direction`` input ranges is either __a__ (Across) or __d__ (Down).
* The ``word`` input can be either a single letter or many letters in succession.

4. To execute a swap on the Scrabble board, you will need to give the following 
inputs ``swap [letter 1] [letter 2] ... [letter n]``
For example, ``swap a b c``

4. To execute a pass on the Scrabble board, you will simply need to type ``pass``
