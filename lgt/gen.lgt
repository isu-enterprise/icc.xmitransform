:- use_module(library(writef)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  This module is a set of structures generation routines, creating them out of blocks.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





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
    ::assertz(option_(Option=Value)).

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
    ::assertz(current_setup(Setup)).

option(Name, Value, Default):-
    ::current_setup(Setup),
    Setup::option(Name, Value, Default).

option(Name, Value):-
    ::current_setup(Setup),
    Setup::option(Name, Value).

set_indent(Number):-
    ::retractall(indent_(_)),
    ::assertz(indent_(Number)).

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
                 render_to/1,
                 remove/1,
                 item/1,
                 items/1
             ]).
:- dynamic([item_/1, block_/1, query_/1, reference_/1]).
:- private([item_/1, block_/1, query_/1, reference_/1]).
:- protected([renderitem/2, render_to/2]).

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

:- public(set_block/1).
set_block(Structure):-
    ::assertz(block_(Structure)).

:- public(current_block/1).
current_block(Structure):-
    ::block_(Structure).

:- public(set_query/1).
set_query(Query):-
    ::assertz(query_(Query)).

:- public(current_query/1).
current_query(Query):-
    ::query_(Query).

:- public(set_reference/1).
set_reference(Reference):-
    ::assertz(reference_(Reference)).

:- public(current_reference/1).
current_reference(Reference):-
    ::reference_(Reference).

clear:-
    ::retractall(item_(_)),
    ::retractall(query_(_)),
    ::retractall(block_(_)).

render(_):-
    writef::writef("ERROR: Implement render/1 by a subclass!\n"),
    fail.

:- public(simple_render/1).
simple_render(Result):-
    findall(R, (::item(Item),
                ::renderitem(Item, R)),
            Result).

render_to(Stream):-
    ::render(List),
    ::render_to(List, Stream).

render_to(List, Stream):-
    lists::is_list(List),!,
    forall(lists::member(X,List), ::render_to(X, Stream)).

render_to(X,Stream):-
    (var(Stream);
     Stream\=nil),
    !,
    write(X),nl.
render_to(_,nil).

:- public(add_skip/1).
add_skip(NumberLines):-
    ::append(empty_lines(NumberLines)).

renderitem(Object, String):-
    current_object(Object), !,
    Object::render(String).
renderitem(literal(Item), String):-!,
    atom_string(Item, String).
renderitem(empty_lines(1), [""]):-!.
renderitem(empty_lines(Number), [""|Rest]):-
    Number>1, !,
    N2 is Number-1,
    renderitem(empty_lines(N2), Rest).
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
    writef::swritef(Result, '(self, %w)', [SObject]).

renderitem(body(Body), StringList):-!,
    Body::render(StringList).

renderitem(Item, Result):-
    ^^renderitem(Item, Result).

separator_option(method_type_separator, ' -> ').

render(Result):-
    (
        ::item(params(Params)),
        ::renderitem(params(Params), SParams);
        SParams='(self)'
    ),
    ^^render(SParams, Sigpart),
    root::iswritef(ISignature, 'def %w:', [Sigpart]),
    root::indent,
    (
        ::item(body(Body)) ->
        ::renderitem(body(Body), LBody);
        root::iswritef(Pass, 'pass', []),
        LBody=[Pass]
    ),
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
    (
        ::item(classlist(List)) ->
        List::render(ClassList),
        root::iswritef(Signature,'class %w(%w):',[Name, ClassList]);
        root::iswritef(Signature,'class %w:',[Name])
    ),
    root::indent,
    (
        ::item(attributes(Attributes))->
        Attributes::render(DefAttrList),
        root::iswritef(ConstructorDef,'def __init__(self, %w):',
                       [DefAttrList]),
        root::indent,
        Attributes::items(InstanceAttrs),
        findall(S,
                (
                    lists::member(Attr, InstanceAttrs),
                    Attr::item(name(AttrName)),
                    root::iswritef(S, "self.%w=%w",
                                   [AttrName, AttrName])
                ),
                AttrAssigns),
        root::unindent,
        AttrList=[ConstructorDef|AttrAssigns];
        root::iswritef(ConstructorDef,'def __init__(self): # Empty constructor', []),
        root::indent,
        root::iswritef(Pass,'pass', []),
        root::unindent,
        AttrList=[ConstructorDef, Pass]),
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


% -------------------- Java class generator -------------------------------------

:- object(java_import,
          specializes(code_block)).

:- public(add/1).
add(Import):-
    ::append(import(Import)).

:- protected(renderitem/2).
renderitem(import(Item), Result):-!,
    root::iswritef(Result, 'import %w;', [Item]).

renderitem(Item, String):-
    ^^renderitem(Item, String).

:- public(render/1).
render(String):-
    ::simple_render(String).

:- end_object.


:- object(java_module,
         specializes(code_block)).

:- public(set_package/1).
set_package(Name):-
    ::append(package(Name)).

renderitem(package(Name), String):-!,
    root::iswritef(String, 'package %w;', [Name]).

renderitem(imports(Object), String):-!,
    Object::render(String).

renderitem(Item, String):-
    ^^renderitem(Item, String).

:- public(render/1).
render(Result):-
    ::simple_render(Result).

:- public(add_imports/1).
add_imports(Object):-
    ::append(imports(Object)).

:- public(imports/1).
imports(Object):-
    ::item(imports(Object)).

:- public(preamble/0).
preamble. % Do nothing

:- end_object.

:- object(java_class,
          specializes(code_block),
          imports(named)).

:- public(render/1).
render([CD,"",A,"",M,"}"]):-
    ::render_class_def(CD),
    root::indent,
    ::render_attributes(A),
    ::render_methods(M),
    root::unindent.

:- public(render_class_def/1).
render_class_def(String):-
    ::item(name(Name)),
    ::renderitem(name(Name), NameString),!,
    (
        ::item(extends(ParentClass)) -> ::renderitem(extends(ParentClass), ParentClassString);
        ParentClassString=""
    ),
    % FIXME: render throws
    ThrowsString="",
    root::iswritef(String, 'class %w%w%w {', [NameString, ParentClassString, ThrowsString]).

render_class_def("// FATAL: cannot render class definition, (no name set)").

:- public(render_attributes/1).
render_attributes(String):-
    (::item(attributes(O)) -> O::render(String);
    root::iswritef(String, '// Attributes', [])).

:- public(render_methods/1).
render_methods(String):-
    (::item(methods(O)) -> O::render(String);
    root::iswritef(String, '// Methods', [])).

:- protected(renderitem/2).
renderitem(extends(Class), Result):-!,
    root::iswritef(Result, ' extends %w', [Class]).
renderitem(Item, Result):-
    ^^renderitem(Item, Result).

:- public(extends/1).
extends(ClassName):-
    ::append(extends(ClassName)).

:- public(attributes/1).
attributes(Object):-
    ::append(attributes(Object)).

:- public(method/1).
method(Structure):-
    ::item(methods(Methods)),
    Methods::append(Structure).

:- end_object.


:- object(java_attributes,
         specializes(code_block)).

:- public(render/1).
render(String):-
    ::simple_render(String).

:- end_object.

:- object(java_methods,
          specializes(code_block)).

:- public(render/1).
render(String):-
    ::simple_render(String).

:- protected(override/1).
override(S):-
    root::iswritef(S,'@Override',[]).

:- protected(end_java_block/1).
end_java_block(S):-
    root::iswritef(S,'}',[]).

:- end_object.

:- object(java_method_body,
          specializes(code_block)).

:- public(render/1).
render(String):-
    ::simple_render(String).

:- end_object.

% -------------------- Java class generator for Rapid Miner ---------------------

% -------------------- Mothur Operator Java class Generator ---------------------

:- object(mothur_attributes,
          specializes(java_attributes)).

:- protected(renderitem/2).
renderitem(input_parameter(Name), String):-!,
    root::iswritef(String, 'private InputPort %wInPort = getInputPorts().createPort("%w");',
                   [Name,Name]).
renderitem(output_parameter(Name), String):-!,
    root::iswritef(String, 'private OutputPort %wOutPort = getOutputPorts().createPort("%w");',
                   [Name,Name]).
renderitem(A,B):-
    ^^renderitem(A,B).

:- end_object.

:- object(mothur_methods,
          specializes(java_methods)).

:- public(renderitem/2).
renderitem(mothur_constructor(Class),[S1,Super,Stodo,E]):-!,
    Class::item(name(ClassName)),
    root::iswritef(S1, 'public %w (OperatorDescription description) {',
                   [ClassName]),
    root::indent,
    root::iswritef(Super,'super(description);'),
    root::iswritef(Stodo,'// TODO Auto-generated constructor stub'),
    root::unindent,
    ::end_java_block(E).

renderitem(mothur_do_work(Class),['',Override,Signature,Super,
                                  Getting,
                                  Process,
                                  Deliver,
                                  Stodo,E]):-!,
    ::override(Override),
    root::iswritef(Signature, 'public void doWork() throws OperatorException {',
                   []),
    root::indent,
    root::iswritef(Super,'super.doWork();'),
    ::get_data_from_ports(Class,Getting),
    ::process_inputs(Class,Process),
    ::deliver_out_ports(Class,Deliver),
    root::iswritef(Stodo,'// TODO to be implemented'),
    root::unindent,
    ::end_java_block(E).

renderitem(mothur_get_parameter_types(Class),['',Override,Signature,Definition,Super,
                                              Stodo,E]):-!,
    ::override(Override),
    root::iswritef(Signature, 'public List<ParameterType> getParameterTypes() {',
                   []),
    root::indent,
    root::iswritef(Definition,'List<ParameterType> types = super.getParameterTypes();'),
    root::iswritef(Stodo,'// TODO to be implemented'),
    root::iswritef(Super,'return type;'),
    % Class::item(attributes(Attributes)),
    % Attributes::render(Ini),
    root::unindent,
    ::end_java_block(E).

renderitem(mothur_get_output_pattern(Query,Class,PSM),
           ['',Override,Signature,Stodo,
            Ifs,Stodo2,Super,E]):-!,
    ::override(Override),
    root::iswritef(Signature, 'public String getOutputPattern(String type) {',
                   []),
    root::indent,
    root::iswritef(Stodo,'// TODO Use a dictionary to reflect type to pattern'),
    findall(
        IfString,
        (PSM::type_pattern(Type,Pattern,Query),
         root::iswritef(IfString,'if (type=="%w") return "%w";',
                       [Type,Pattern])),
        Ifs),
    root::iswritef(Stodo2,'// TODO if nil then '),
    root::iswritef(Super,'return super.getOutputPattern(type);'),
    root::unindent,
    ::end_java_block(E).

renderitem(A,B):-
    ^^renderitem(A,B).

:- public(deliver_out_ports/2).
deliver_out_ports(Class, Result):-
    Class::current_reference(module(Module)),
    Module::current_query(Query),
    root::iswritef(Result,'// TODO:Delivers', []).

:- public(get_data_from_ports/2).
get_data_from_ports(Class, Result):-
    Class::current_reference(module(Module)),
    Module::current_query(Query),
    root::iswritef(Result,'// TODO:Getting', []).

:- public(process_inputs/2).
process_inputs(Class, Result):-
    Class::current_reference(module(Module)),
    Module::current_query(Query),
    root::iswritef(Result,'// TODO:Process', []).

:- end_object.

:- object(mothur_class,
          specializes(java_class)).

:- public(input_parameter/1).
input_parameter(Name):-
    ::item(attributes(A)),
    A::append(input_parameter(Name)).

:- public(output_parameter/1).
output_parameter(Name):-
    ::item(attributes(A)),
    A::append(output_parameter(Name)).

:- public(preamble/0).
preamble:-
    create_object(Attributes, [instantiates(mothur_attributes)],[],[]),
    ::append(attributes(Attributes)),
    ::set_block(attributes(Attributes)),
    create_object(Methods, [instantiates(mothur_methods)],[],[]),
    ::append(methods(Methods)),
    ::set_block(methods(Methods)),
    default_mothur_method_set,
    true.

:- public(default_mothur_method_set/0).
default_mothur_method_set:-
    self(Self),
    ::method(mothur_constructor(Self)),
    ::method(mothur_do_work(Self)),
    ::method(mothur_get_parameter_types(Self)).
    % create_object(Constructor, [instantiates(method_body)],[],[]),
    % ::method(mothur_constructor(Constructor)).

:- end_object.

:- object(mothur_module,
          specializes(java_module)).


:- public(preamble/0).
preamble:-
    ^^preamble,
    ::set_package('com.rapidminer.ngs.operator'),
    ::add_skip(1),
    create_object(Imports, [instantiates(java_import)],[],[]),
    ::set_block(imports(Imports)),
    ::append(Imports),
    ::add_skip(1),
    create_object(ClassDef, [instantiates(mothur_class)],[],[]),
    ClassDef::preamble,
    ::set_block(class(ClassDef)),
    ::append(ClassDef),
    Imports::add('com.rapidminer.operator.OperatorDescription'),
    Imports::add('com.rapidminer.operator.ports.InputPort'),
    Imports::add('com.rapidminer.operator.ports.OutputPort'),
    ClassDef::extends('MothurGeneratedOperator').

:- end_object.
