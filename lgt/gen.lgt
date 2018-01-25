
:- object(metaclass, instantiates(metaclass)).
:- end_object.

:- object(class, instantiates(metaclass)).
:- end_object.

:- object(setup).
:- public([
                 option/1,
                 option/2,
                 options/1, % as list
                 clear/0,
                 remove/1,
                 set/2,   % Set an option to value
                 set/1,   % Set in form of <option>=<value>
                 get/2    % get option value
             ]).
:- protected([
                    option_/1  % An option in form of option_(<option>=<value>).
                ]).
:- dynamic([
                  option_/1
              ]).
set(Option=Value):-
    ::set(Option, Value).
set(Option/Value):-
    ::set(Option, Value).
set(Option-Value):-
    ::set(Option, Value).

set(Option, Value):-
    ::remove(Option),
    ::assert(option_(Option=Value)).

get(Option, Value):-
    ::option_(Option=Value),!.

remove(Option):-
    ::retractall(option_(Option=_)).

clear:-
    ::retractall(option_(_)).

option(Option,Value):-
    ::option_(Option=Value).
option(Option):-
    ::option_(Option).

options(List):-
    findall(O, ::option_(O), List).

:- end_object.

:- object(code_block, specializes(class)).
:- public([
                 append/1,
                 prepend/1,
                 clear/0,
                 render/1,
                 item/1
             ]).
:- dynamic([
                  item_/1
              ]).
:- private([
                  item_/1
              ]).

item(Item):-
    ::item_(Item).

append(Item):-
    ::assertz(item_(Item)).

prepend(Item):-
    ::asserta(item_(Item)).

clear:-
    ::retractall(item_(_)).

render(Setup):-
    Setup::get(tab_size, Size),
    Size>=0.

:- end_object.
