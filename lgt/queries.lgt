
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
