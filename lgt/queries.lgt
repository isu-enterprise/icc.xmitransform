
% Query module

:- object(query(_XMI)).

:- protected(xmi/1).
:- public([class/2, attribute/3, method/3]).

xmi(XMI):-
    parameter(1, XMI).

class(Name, ID):-
    ::xmi(XMI),
    XMI::rdf(ID,rdf:type,uml,'Class'),
    XMI::rdf(ID,rdfs:label, literal(Name)).

attribute(Name, ClassID, ID):-
    ::xmi(XMI),
    XMI::graph(G),
    XMI::rdf(ClassID, G:ownedAttribute, ID),
    % XMI::rdf(ID, rdf:type, uml,'Property'), % this can be a type
    XMI::rdf(ID, rdfs:label, literal(Name)).

method(Name, ClassID, ID):-
    ::xmi(XMI),
    XMI::graph(G),
    XMI::rdf(ClassID, G:ownedOperation, ID),
    XMI::rdf(ID, rdfs:label, literal(Name)).


:- end_object.


% Querying NGS RDF Graphs for modules.

:- object(queryparam(_RDF,_Parameter)).

:- protected(ngs/1).
ngs(RDF):-
    parameter(1, RDF).

:- protected(parameter/1).
parameter(Parameter):-
    parameter(2,Parameter).

:- public(type/1).
type(Type) :-
    ::attr(type, Type).

:- public(name/1).
name(Name) :-
    ::attr(dc:title, literal(Name)).

:- public(options/1).
options(Value):-
    ::attr(options, Value).

:- public(options_default/1).
options_default(Value):-
    ::attr(optionsDefault, Value).

:- public(choose_only_one_group/1).
choose_only_one_group(Value):-
    ::attr(chooseAtLeastOneGroup, Value).

:- public(choose_at_least_one_group/1).
choose_at_least_one_group(Value):-
    ::attr(chooseAtLeastOneGroup, Value).

:- public(linked_group/1).
linked_group(Value):-
    ::attr(linkedGroup, Value).

:- public(output_types/1).
output_types(Types):-
    ::attr(outputTypes, Types).

:- public(multiple_selection_allowed/0).
multiple_selection_allowed:-
    ::bool_attr(multipleSelectionAllowed).

:- public(required/0).
required:-
    ::bool_attr(required).

:- public(important/0).
important:-
    ::bool_attr(important).

:- protected(attr/2).
attr(NS:Name, Value):-
    ::ngs(RDF),
    ::parameter(Parameter),
    RDF::rdf(Parameter, NS:Name, Value).

attr(Name, Value):-
    \+ Name=_:_,!,
    ::ngs(RDF),
    ::parameter(Parameter),
    RDF::rdf(Parameter, ngsp:Name, Value).

:- protected(bool_attr/1).
bool_attr(Name):-
    ::attr(Name, literal(type('http://www.w3.org/2001/XMLSchema#boolean',true))),!.
bool_attr(Name):-
    ::attr(Name, literal(type('http://www.w3.org/2001/XMLSchema#boolean',false))),!,
    fail.
bool_attr(Name):-
    ::attr(Name, Value),
    ::parameter(Parameter),
    writef::writef('WARNING: attr %w of %w has wring bool value: %w (assuming false)\n',
                   [Name, Parameter, Value]),!,
    fail.

:- end_object.

:- object(queryngs(_RDF)).
:- protected([ngs/1]).
:- public([module/2]).

ngs(RDF):-
    parameter(1, RDF).

module(Name, Module):-
    ::ngs(RDF),
    % RDF::graph(Mothur),
    ::mothur(Mothur),
    RDF::rdf(Mothur, ngsp:module, Module),
    RDF::rdf(Module, dc:title, literal(Name)).

:- public(mothur/1).
mothur(RES):-
    ::ngs(RDF),
    RDF::rdf(RES, rdf:type, ngsp, 'Specification').

:- public(parameter/3).
parameter(Module, Parameter, ParameterName):-
    ::ngs(RDF),
    RDF::rdf(Module, ngsp:parameter, Parameter),
    RDF::rdf(Parameter, rdf:type, ngsp, 'Parameter'),
    RDF::rdf(Parameter, dc:title, literal(ParameterName)).

:- public(parameter/4).
parameter(Module, Parameter, ParameterName, queryparam(RDF,Parameter)):-
    ::ngs(RDF),
    ::parameter(Module, Parameter, ParameterName).

:- end_object.
