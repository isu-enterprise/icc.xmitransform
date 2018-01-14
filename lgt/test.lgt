:- object(list).

    :- public(member/2).
    member(Head, [Head| _]).
    member(Head, [_| Tail]) :-
        member(Head, Tail).

:- end_object.
