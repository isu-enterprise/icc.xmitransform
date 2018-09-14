
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

:- object(queryngs(_RDF)).
:- protected([ngs/1]).
:- public([module/2, parameter/3]).

ngs(RDF):-
    parameter(1, RDF).

module(Name, ID):-
    ::ngs(RDF),
    % RDF::graph(Mothur),
    ::mothur(Mothur),
    RDF::rdf(Mothur, ngsp:module, Module),
    RDF::rdf(Module, dc:title, Name).

:- public(mothur/1).
mothur(RES):-
    ::ngs(RDF),
    RDF::rdf(RES, rdf:type, ngsp, 'Specification').

:- end_object.
