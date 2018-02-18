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

:- object(genmetaclass, instantiates(genmetaclass)).
:- public([setup/1,
           indent/1,
           indent/2,
           indent/0,
           unindent/1,
           unindent/0,
           iswritef/3,
           iswritef/2,
           option/2,
           option/3,
           clear_indent/0
          ]).
:- private([indent_/1,
            indent_str/2,
            indent_str_/2]).
:- protected([current_setup/1,
              indentstr/1,
              set_indent/1
             ]).
:- dynamic([indent_/1, current_setup/1]).
:- initialization(::clear_indent).

setup(Setup):-
    ::retractall(current_setup(_)),
    ::assert(current_setup(Setup)).

option(Name, Value, Default):-
    ::current_setup(Setup),
    Setup::option(Name, Value, Default).

option(Name, Value):-
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
iswritef(String, Pattern):-
    ::iswritef(String, Pattern, []).

indent_str("\t"):-
    ::option(use_tabs,true,true),
    !.
indent_str(S):-
    ::option(use_tabs,false,true),!,
    ::option(tab_size,Size,8),!,
    indent_str_(Size, S).

indent_str_(0, "").
indent_str_(N, String):-
    N>0,
    ::option(indent_char, C, " "),
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

:- object(root, instantiates(genmetaclass)).
:- end_object.

:- category(listrenderable).

:- protected([
              list_separator/1,
              separator_option/2
             ]).
:- private([renderitems/3]).
:- public([renderaslist/2,
           render/1,
           render/2
          ]).

renderaslist(Separator, String):-
    ::items(Items),!,
    ::renderitems(Items, Separator, String).
renderaslist(_,_,""):-
    writef::writef('ERROR: The list is empty, nothing to render!\n'),
    fail.

renderitems([],_,"").
renderitems([A], _, SA):-
    ::renderitem(A, SA).
renderitems([A,B|T], Separator, String):-
    ::renderitem(A, SA),
    string_concat(SA, Separator, SAS),
    ::renderitems([B|T], Separator, BTS),
    string_concat(SAS, BTS, String).

render(Separator, Result):-
    ::renderaslist(Separator, Result).

render(Result):-
    ::list_separator(Separator),
    ::render(Separator, Result).

list_separator(Separator):-
    ::separator_option(Name, Default),!,
    root::option(Name, Separator, Default).

:- end_category.


:- object(code_block, specializes(root)).
:- public([
                 append/1,
                 prepend/1,
                 clear/0,
                 render/1,
                 remove/1,
                 item/1,
                 items/1
             ]).
:- dynamic([item_/1]).
:- private([item_/1]).
:- protected([renderitem/2]).

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

render(_):-
    writef::writef("ERROR: Implement render/1 by a subclass!\n"),
    fail.

renderitem(Object, String):-
    current_object(Object), !,
    Object::render(String).
renderitem(literal(Item), String):-!,
    atom_string(Item, String).
renderitem(Item, String):-
    root::iswritef(String, '%q', [Item]).

:- end_object.

:- category(named).
:- public([name/1, render/1]).
:- protected([renderitem/2]).

name(Name):-
    ::prepend(name(Name)).

renderitem(name(Name), String):-!,
    atom_string(Name, String).

render(String):-
    ::item(name(Name)),
    ::renderitem(name(Name), String).

:-end_category.

:- category(namedtyped, extends(named)).
:- public([
                 type/1,
                 render/2,
                 separator_option/2,
                 list_separator/1
             ]).
:- protected([renderitem/2]).

type(Type):-
    ::append(type(Type)).

renderitem(Item, String):-
    ^^renderitem(Item, String),!.
renderitem(type(Type),String):-!,
    ::list_separator(Separator),
    writef::swritef(String, '%w%w', [Separator, Type]).

render(Middle, String):-
    ^^render(SName),
    (
        ::item(type(Type)) ->
        ::renderitem(type(Type), SType),
        string_concat(SName, Middle, _1),
        string_concat(_1, SType, String) ;
        SName = String
    ).

render(String):-
    ::render("", String).

list_separator(Separator):-
    ::separator_option(Name, Default),!,
    root::option(Name, Separator, Default).

:- end_category.

:- object(param, specializes(code_block), imports([namedtyped])).

:- public([default/1]).

default(Default):-
    ::append(default(Default)).

separator_option(param_type_separator, ':').

renderitem(default(Default), String):-!,
    writef::swritef(String, '=%q', [Default]).
renderitem(Item, String):-
    ^^renderitem(Item, String).

render(Result):-
    ^^render(Beginning),
    (
        ::item(default(Default)) ->
        ::renderitem(default(Default), SDefault),
        string_concat(Beginning, SDefault, Result);
        Result = Beginning
    ).

:- end_object.


:- object(params, specializes(code_block), imports(listrenderable)).

separator_option(param_list_separator, ', ').

renderitem(Item, Result):-
    ^^renderitem(Item, Result).

:- end_object.

:- object(dottedname, specializes(params)).

separator_option(dotted_name_separator, '.').

:- end_object.

:- object(body, specializes(code_block)).
render(List):-
    findall(L, (::item(X),^^renderitem(X,L)), List).
:- end_object.

:- object(method, specializes(code_block), imports(namedtyped)).
:- public([params/1, body/1]).
body(X):-
    ::append(body(X)).
params(X):-
    ::append(params(X)).

renderitem(params(Object), Result):-!,
    Object::render(SObject),
    writef::swritef(Result, '(%w)', [SObject]).

renderitem(body(Body), StringList):-!,
    Body::render(StringList).

renderitem(Item, Result):-
    ^^renderitem(Item, Result).

separator_option(method_type_separator, ' -> ').

render(Result):-
    ::item(params(Params)),
    ::renderitem(params(Params), SParams),
    ^^render(SParams, Sigpart),
    root::iswritef(ISignature, 'def %w:', [Sigpart]),
    root::indent,
    ::item(body(Body)),
    ::renderitem(body(Body), LBody),
    root::unindent,
    Result=[ISignature | LBody].

:- end_object.

:- object(classlist, specializes(code_block), imports([listrenderable])).

separator_option(class_list_separator, ", ").

:- end_object.

:- object(methodlist, specializes(code_block)).

render(List):-
    findall(Result, (::item(Item), ::renderitem(Item, Result)), List).

:- end_object.

:- object(class, specializes(code_block), imports([named])).

:- public([classlist/1, methods/1, attributes/1]).

classlist(ClassList):-
    ::prepend(classlist(ClassList)).

attributes(Attributes):-
    ::prepend(attributes(Attributes)).

methods(MethodList):-
    ::append(methods(MethodList)).

renderitem(Item, Result):-
    ^^renderitem(Item, Result).

render(Result):-
    ^^render(Name),
    ::item(classlist(List)),
    List::render(ClassList),
    % writef::writef('---> Class List: %w\n',[ClassList]),
    root::iswritef(Signature,'class %w(%w):',[Name, ClassList]),
    root::indent,
    (
        ::item(attributes(Attributes))->
        Attributes::render(AttrList);
        AttrList=[]),
    (
        ::item(methods(Methods))->
        Methods::render(MethodList);
        MethodList=[]),
    lists::append(AttrList, MethodList, StringList),
    root::unindent,
    Result=[Signature | StringList].

:- end_object.

:- object(import, specializes(code_block)).
:- public([import/1, from/2, from/3, importas/2]).
import(Name):-
    ::append(import(Name)).
from(Name, List):-
    ::append(from(Name, List)).
from(Name, Id, As):-
    ::append(from(Name, Id, As)).
importas(Name, As):-
    ::append(import(Name, As)).

renderitem(import(Name), String):-!,
    ^^renderitem(Name, SName),!,
    root::iswritef(String, 'import %w', [SName]).
renderitem(import(Name, As), String):-!,
    ^^renderitem(Name, SName),!,
    root::iswritef(String, 'import %w as %w', [SName, As]).
renderitem(from(Name, List), String):-!,
    ^^renderitem(Name, SName),!,
    ^^renderitem(List, SList),!,
    root::iswritef(String, 'from %w import %w', [SName, SList]).
renderitem(from(Name, Id, As), String):-
    ^^renderitem(Name, SName),!,
    ^^renderitem(Id, SId),!,
    root::iswritef(String, 'from %w import %w as %w', [SName, SId, As]).

renderitem(Item, String):-
    ^^renderitem(Item, String).

render(List):-
    findall(Result, (::item(Item),
                     ::renderitem(Item, Result)),
            List).

:- end_object.

:- object(module, specializes(code_block)).
% :- public(import/1, class/1,
:- end_object.
