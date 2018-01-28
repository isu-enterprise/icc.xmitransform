:- object(listtest).

    :- public(member/2).
    member(Head, [Head| _]).
    member(Head, [_| Tail]) :-
        member(Head, Tail).

:- end_object.

:- object(metaclass,
    instantiates(metaclass)).
:- end_object.

:- category(pcat).
:- protected([
                    a/1
                ]).
:- public([
                 p/1
             ]).

p(X):-
    ::a(X).

a(cat).

:- end_category.


:- object(aclass, specializes(metaclass), imports(pcat)).
:- public([
                 b/1
             ]).

b(X):-
    ::a(X).

a(a).
a(X):-
    ^^a(X).

:- end_object.


:- object(bclass, specializes(aclass)).

a(b).
a(X):-
    ^^a(X).

:- end_object.


:- object(a, instantiates(aclass)).
:- end_object.

:- object(b, instantiates(bclass)).
:- end_object.
