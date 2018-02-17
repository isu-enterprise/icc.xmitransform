% Load modules

:- logtalk_load(['../queries.lgt', '../xmi.lgt', '../model.lgt']).

:- object(direct(_Model)).
:- public([tr/4,tr/3]).
:- protected([model/1, package/1, profiles/2]).
:- dynamic(xmi_model/4).

model(Model):-
    parameter(1, Model).

package(Package):-
    ::model(Model),
    xmi_model(Model, Package, _, _).

profiles(Local, Code):-
    ::model(Model),
    xmi_model(Model, _, Local, Code).

tr(class, Class, ClassID):-
    ::package(Package),
    query(Package)::class(Name, ClassID),
    create_object(Class, [instantiates(class)],[],[]),
    create_object(Attributes, [instantiates(params)],[],[]),
    create_object(Methods, [instantiates(methods)],[],[]),
    Class::name(Name),
    forall(::tr(attribute, Attribute, ClassID, _AttributeID),
           Attributes::append(Attribute)
          ),
    forall(::tr(method, Method, ClassID, _MethodID),
           Methods::append(Method)
          ),
    Class::attributes(Attributes),
    Class::methods(Methods).

tr(attribute, Attribute, ClassID, AttributeID):-
    ::package(Package),
    query(Package)::class_attribute(Name, ClassID, AttributeID),
    query(Package)::attribute_type(Type, AttributeID),
    create_object(Attribute, [instantiates(param)],[],[]),
    Attribute::name(Name),
    Attribute::type(Type).

tr(method, Method, ClassID, MethodID):-
    ::package(Package),
    query(Package)::class_method(Name, ClassID, MethodID),
    create_object(Method, [instantiates(method)],[],[]),
    Method::name(Name).

:- end_object.
