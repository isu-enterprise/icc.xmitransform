
:- object(metaclass, instantiates(metaclass)).
:- end_object.

:- object(class, instantiates(metaclass)).
:- end_object.

:- object(code_block, specializes(class)).
:- public([
                 append/1,
                 prepend/1,
                 item/1

             ]).
:- dynamic([
                  item_/1
              ]).
:- private([
                  item_/1
              ]).

item(Item):-
    item_(Item).

append(Item):-
    ::assertz(item_(Item)).

prepend(Item):-
    ::asserta(item_(Item)).

clear:-
    ::retractall(item_(_)).


:- end_object.
