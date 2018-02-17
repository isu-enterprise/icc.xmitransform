
% Model module
% Load queries

:- logtalk_load(queries).

:- object(profiles(_LocalProfile, _CodeProfile)).
:- end_object.

:- object(model(_Package, _Profiles)).
:- end_object.

:- dynamic(xmi_model/4).
:- discontiguous(xmi_model/4).
