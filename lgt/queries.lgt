
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

ngs(RDF):-
    parameter(1, RDF).

parameter(Parameter):-
    parameter(2,Parameter).

:- public(type/1).
type(Type) :-
    ngs(RDF),
    parameter(Parameter),
    RDF::rdf(Parameter, ngsp:type, Type).

:- public(name/1).
name(Name) :-
    ngs(RDF),
    parameter(Parameter),
    RDF::rdf(Parameter, dc:title, literal(Name)).
:- end_object.

:- object(queryngs(_RDF)).
:- protected([ngs/1]).
:- public([module/2, parameter/3]).

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
