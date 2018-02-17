
% Query module

:- object(query(_XMI)).

:- protected(xmi/1).

xmi(XMI):-
    ::parameter(1, XMI).

:- end_object.
