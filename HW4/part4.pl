%part1
element(E,[First|Rest]) :-
    First == E ; element(E,Rest).


% part 2
unionSets([],X,X).

unionSets([E|RestS1], S2, S3):-
     unionSets(RestS1,[E|S2],S3).

union(S1,S2,S3) :-
    unionSets(S1,S2,UnionSet),
    equivalent(S3,UnionSet).

%part3
intersectionSets([],_,X,X).

intersectionSets([E|RestS1], S2, S3,Empty):-
    (element(E,S2),intersectionSets(RestS1,S2,S3,[E|Empty]));
    (not(element(E,S2)),intersectionSets(RestS1,S2,S3,Empty)).

intersect(S1,S2,S3):-
    intersectionSets(S1,S2,IntersectionSet,[]),
    equivalent(S3,IntersectionSet).

%Part4
equivalent(S1,S2) :-
   equivalentHelper(S1,S2),
   equivalentHelper(S2,S1).

equivalentHelper([],_).

equivalentHelper([E|RestS1],S2) :-
       element(E,S2), equivalentHelper(RestS1,S2).
