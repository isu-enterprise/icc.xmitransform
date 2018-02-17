
% Query module

:- object(query(_XMI)).

:- protected(xmi/1).

xmi(XMI):-
    parameter(1, XMI).

class(Name, ID):-
    ::xmi(XMI),
    XMI::rdf(_,_,_).

:- end_object.
