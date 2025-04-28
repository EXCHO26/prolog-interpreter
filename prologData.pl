parent(john, mary).
parent(mary, susan).
parent(susan, ben).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).