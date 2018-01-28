:- use_module(library(writef)).


:- object(metaclass, instantiates(metaclass)).
:- end_object.

:- object(class, instantiates(metaclass)).
:- end_object.

:- object(setup).
:- public([
                 option/1,
                 option/2,
                 option/3,
                 options/1, % as list
                 clear/0,
                 remove/1,
                 set/2,   % Set an option to value
                 set/1   % Set in form of <option>=<value>
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

remove(Option):-
    ::retractall(option_(Option=_)).

clear:-
    ::retractall(option_(_)).

option(Option, Value):-
    ::option_(Option=Value).
option(Option, Value, _):-
    ::option(Option,Value),!.
option(Option, Default, Default):-
    ::option_(Option=_).
option(Option):-
    ::option_(Option).

options(List):-
    findall(O, ::option_(O), List).

:- end_object.

:- category(listrenderable).

:- protected([renderitem/3]).
:- private([renderitems/4]).
:- public([renderaslist/3]).

renderaslist(Setup, Separator, String):-
    ::items(Items),!,
    ::renderitems(Items, Setup, Separator, String).
renderaslist(_,_,"").

renderitems([],_,_,"").
renderitems([A], Setup, _, SA):-
    ::renderitem(A, Setup, SA).
renderitems([A,B|T], Setup, Separator, String):-
    ::renderitem(A, Setup, SA),
    string_concat(SA, Separator, SAS),
    ::renderitems([B|T], Setup, Separator, BTS),
    string_concat(SAS, BTS, String).

renderitem(Item, Setup, String):-
    % writef::writef("Object....?: %w",[Item]),
    current_object(Item),!,
    Item::render(Setup, String).

:- end_category.



:- object(code_block, specializes(class)).
:- public([
                 append/1,
                 prepend/1,
                 clear/0,
                 render/2,
                 remove/1,
                 item/1,
                 items/1
             ]).
:- dynamic([
                  item_/1
              ]).
:- private([
                  item_/1
              ]).
:- protected([
                ]).

item(Item):-
    ::item_(Item).

items(Items):-
    bagof(Item, ::item(Item), Items).

append(Item):-
    ::assertz(item_(Item)).

prepend(Item):-
    ::asserta(item_(Item)).

remove(Item):-
    ::retract(item_(Item)).

clear:-
    ::retractall(item_(_)).

render(_Setup, _String).
:- end_object.


:- object(param, specializes(code_block), imports(listrenderable)).

:- protected([
                  renderitem/3
              ]).
:- public([
                 name/1,
                 type/1,
                 default/1
             ]).

name(Name):-
    ::prepend(name(Name)).
type(Type):-
    ::append(type(Type)).
default(Default):-
    ::append(default(Default)).

%% renderitem(I, Setup, ""):-
%%     writef::writef("---- Call: %w,%w",[I, Setup]),
%%     fail.
renderitem(name(Name), _Setup, String):-!,
    atom_string(Name, String).
renderitem(type(Type), _Setup, String):-!,
    writef::swritef(String, ':%w', [Type]).
renderitem(default(Default), _Setup, String):-!,
    writef::swritef(String, '=%q', [Default]).
renderitem(Item, Setup, String):-
    ^^renderitem(Item, Setup, String).

render(Setup, Result):-
    ::renderaslist(Setup, "", Result).

:- end_object.


:- object(params, specializes(code_block), imports(listrenderable)).
:- end_object.
