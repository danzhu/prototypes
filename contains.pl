contains([Val | _], Val).
contains([V | Set], Val) :- V \= Val, contains(Set, Val).

contains(Map, Key, Val) :- contains(Map, (Key, V)), V = Val.

cap([]) :- !.
cap([_ | L]) :- cap(L).

git(Path) :- contains(Path, '/usr/bin').
vim(Env) :- contains(Env, 'EDITOR', vim).
emacs(Env) :- contains(Env, 'EDITOR', emacs).

main(Path, Env) :-
    git(Path),
    emacs(Env),
    cap(Path),
    cap(Env).
