%facts..
when_(102, 10).
when_(108, 12).
when_(341, 14).
when_(455, 16).
when_(452, 17).

where(102, z23).
where(108, z11).
where(341, z06).
where(455, z07).
where(452, z07).

enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

%rules..

schedule(S, P, T) :-
    enroll(S,Class), where(Class,P), when_(Class, T).

usage(P,T) :-
    where(Class,P), when_(Class, T).

conflict(X,Y) :-
    (where(X,Px), where(Y,Py), Px==Py).

conflict(X,Y) :-
    when_(X,Tx), when_(Y,Ty), Tx==Ty.

meet(X,Y) :-
    enroll(X,Cx), enroll(Y, Cy),
    where(Cx,Px), where(Cy,Py),
    when_(Cx,Tx), when_(Cy,Ty),
    Px==Py, Tx==Ty.
