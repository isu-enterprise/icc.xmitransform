:- use_module(library(writef)).

%%%%%%%%%%%%%%%%%%%% Global setup object prototype %%%%%%%%%%%%%%%%%%%%5

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

option(Option, Value, Default):-
    var(Value),
    (
        ::option(Option, Value) ->
        true;
        Value = Default
    ),!.

option(Option, Value, Default):-
    nonvar(Value),
    (
        ::option(Option, _Value) ->
        (
            Value = _Value ->
            true;
            fail
        );
        Value = Default
    ),!.


option(Option):-
    ::option_(Option).

options(List):-
    findall(O, ::option_(O), List).

:- end_object.

%%%%%%%%%%%%%%%%%%%% A class hierarchy of code blocks %%%%%%%%%%%%%%%%%%%%5

:- object(metaclass, instantiates(metaclass)).
:- public([setup/1,
           indent/1,
           indent/2,
           indent/0,
           unindent/1,
           unindent/0,
           iswritef/3
          ]).
:- private([indent_/1,
            indent_str/2,
            indent_str_/2]).
:- protected([current_setup/1,
              option_/2,
              option_/3,
              indentstr/1,
              set_indent/1,
              clear_indent/0
             ]).
:- dynamic([indent_/1, current_setup/1]).
:- initialization(::clear_indent).

setup(Setup):-
    ::retractall(current_setup(_)),
    ::assert(current_setup(Setup)).

option_(Name, Value, Default):-
    ::current_setup(Setup),
    Setup::option(Name, Value, Default).

option_(Name, Value):-
    ::current_setup(Setup),
    Setup::option(Name, Value).

set_indent(Number):-
    ::retractall(indent_(_)),
    ::assert(indent_(Number)).

clear_indent:-
    ::set_indent(0).

indent(String):-
    ::indent_(Number),
    ::indent(Number, String).

indent(0, "").
indent(N, String):-
    N > 0,!,
    N1 is N-1,
    ::indent(N1, _1),
    indent_str(C),
    string_concat(C, _1, String).

iswritef(String, Pattern, Params):-
    ::indent(Indent),
    writef::swritef(_1, Pattern, Params),
    string_concat(Indent, _1, String).

indent_str("\t"):-
    ::option_(use_tabs,true,true),
    !.
indent_str(S):-
    ::option_(use_tabs,false,true),!,
    ::option_(tab_size,Size,8),!,
    indent_str_(Size, S).

indent_str_(0, "").
indent_str_(N, String):-
    N>0,
    ::option_(indent_char, C, " "),
    N1 is N - 1,
    ::indent_str_(N1, _1),
    string_concat(C, _1, String).

indent:-
    ::indent_(Current),
    New is Current + 1,
    ::set_indent(New).

unindent:-
    ::indent_(Current),
    Current >= 0,
    New is Current - 1,
    ::set_indent(New).

:- end_object.

:- object(class, instantiates(metaclass)).
:- end_object.

:- category(listrenderable).

:- protected([renderitem/3, renderobject/3]).
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
    renderobject(Item, Setup, String),!.
renderitem(Item, _Setup, String):-
    writef::swritef(String, '%q', Item).

renderobject(Object, Setup, String):-
    current_object(Object),!,
    writef::writef("Object: %w\n",[Object]),
    Object::render(Setup, String).

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

render(_Setup, ""):-
    writef::writef("Warning: Default rendering is empty\n").

:- end_object.

:- category(namedtyped).
:- public([
                 name/1,
                 type/1,
                 render/3,
                 render/2
             ]).
:- protected([
                    renderitem/3,
                    type_separator/2
                ]).

name(Name):-
    ::prepend(name(Name)).

type(Type):-
    ::append(type(Type)).

renderitem(name(Name), _Setup, String):-!,
    atom_string(Name, String).
renderitem(type(Type), Setup, String):-!,
    ::type_separator(Setup, Separator),
    writef::swritef(String, '%w%w', [Separator, Type]).

render(Setup, Middle, String):-
    ::item(name(Name)),
    ::renderitem(name(Name), Setup, SName),
    (
        ::item(type(Type)) ->
        ::renderitem(type(Type), Setup, SType),
        string_concat(SName, Middle, _1),
        string_concat(_1, SType, String) ;
        SName = String
    ).

render(Setup, String):-
    ::render(Setup, "", String).

:- end_category.

:- object(param, specializes(code_block), imports([namedtyped, listrenderable])).

:- public([
                 default/1
             ]).

default(Default):-
    ::append(default(Default)).

type_separator(Setup, Value):-
    Setup::option(param_type_separator, Value, ':').

renderitem(default(Default), _Setup, String):-!,
    writef::swritef(String, '=%q', [Default]).
renderitem(Item, Setup, String):-
    ^^renderitem(Item, Setup, String).

%% render(Setup, Result):-
%%     ::renderaslist(Setup, "", Result).
render(Setup, Result):-
    ^^render(Setup, Beginning),
    (
        ::item(default(Default)) ->
        ::renderitem(default(Default), Setup, SDefault),
        string_concat(Beginning, SDefault, Result);
        Result = Beginning
    ).

:- end_object.


:- object(params, specializes(code_block), imports(listrenderable)).

render(Setup, Result):-
    ::renderaslist(Setup, ', ', Result).

:- end_object.

:- object(method, specializes(code_block), imports(namedtyped)).
:- public([params/1, body/1]).
body(X):-
    ::append(body(X)).
params(X):-
    ::append(params(X)).


renderitem(params(Object), Setup, Result):-!,
    Object::render(Setup, SObject),
    writef::swritef(Result, '(%w)', [SObject]).

renderitem(Item, Setup, Result):-
    ^^renderitem(Item, Setup, Result).

type_separator(Setup, Value):-
    Setup::option(method_type_separator, Value, ' -> ').

render(Setup, Result):-
    ::item(params(Params)),
    ::renderitem(params(Params), Setup, SParams),
    ^^render(Setup, SParams, Sigpart),
    writef::swritef(_1, 'def %w:', [Sigpart]),
    Result=_1.

:- end_object.
