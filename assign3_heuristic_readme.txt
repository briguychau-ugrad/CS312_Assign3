Brian Chau
Daniel Lu

Here is a short description of our heuristic algorithm:

We just compared two factors about every pair of states:

Heuristic 1: (# of squares between X-car and exit square) - (# of cars between X-car and exit square): our rationale was that the higher this number is, the more likely it is for the X-car to have a free path to the exit.

Heuristic 2: # of cars on the edges of the grid: our rationale was that the more cars there are on the edges, the more free the centre of the board will be.


To compare two states, we summed up Heuristic 1 and Heuristic 2, and we take the highest heuristic-summed state to search on.
