
% Query module

:- object(query(_XMI)).

:- protected(xmi/1).
:- public([class/2, attr/3]).

xmi(XMI):-
    parameter(1, XMI).

class(Name, ID):-
    ::xmi(XMI),
    XMI::rdf(ID,rdf:type,uml,'Class'),
    XMI::rdf(ID,rdfs:label, literal(Name)).

attr(Name, ClassID, ID):-
    ::xmi(XNI),
    write(1),
    XMI::graph(G),
    write(2),
    XMI::rdf(ClassID, G:ownedAttribute, ID),
    write(3),
    XMI::rdf(ID, rdf:type, uml,'Property'),
    write(4),
    XMI::rdf(ID, rdfs:label, literal(Name)).


:- end_object.
