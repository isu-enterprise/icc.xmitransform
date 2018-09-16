
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

:- object(ngsquerybase).

:- protected(ngs/1).
ngs(RDF):-
    ::first(RDF).

:- end_object.


:- object(queryparam(_RDF,_Parameter),
          extends(ngsquerybase)).

:- protected(first/1).
first(RDF):-
    parameter(1, RDF).

:- protected(second/1).
second(Parameter):-
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
    ::second(Parameter),
    rdf_db::rdf_global_object(Value, V),
    RDF::rdf(Parameter, NS:Name, V).

attr(Name, Value):-
    \+ Name=_:_,!,
    ::ngs(RDF),
    ::second(Parameter),
    rdf_db::rdf_global_id(Value, V),
    RDF::rdf(Parameter, ngsp:Name, V).

:- protected(bool_attr/1).
bool_attr(Name):-
    ::attr(Name, literal(type('http://www.w3.org/2001/XMLSchema#boolean',true))),!.
bool_attr(Name):-
    ::attr(Name, literal(type('http://www.w3.org/2001/XMLSchema#boolean',false))),!,
    fail.
bool_attr(Name):-
    ::attr(Name, Value),
    ::second(Parameter),
    writef::writef('WARNING: attr %w of %w has wring bool value: %w (assuming false)\n',
                   [Name, Parameter, Value]),!,
    fail.

:- end_object.

:- object(querymodule(_RDF,_Module),
          extends(ngsquerybase)).

:- protected(first/1).
first(RDF):-
    parameter(1, RDF).

:- protected(second/1).
second(Parameter):-
    parameter(2,Parameter).

:- public(parameter/3).
parameter(Parameter, ParameterName, queryparam(RDF,Parameter)):-
    ::ngs(RDF),
    ::second(Module),
    RDF::rdf(Module, ngsp:parameter, Parameter),
    RDF::rdf(Parameter, rdf:type, ngsp, 'Parameter'),
    RDF::rdf(Parameter, dc:title, literal(ParameterName)).

:- public(output_pattern/2).
output_pattern(Pattern,Type):-
    ::ngs(RDF),
    ::second(Module),
    RDF::rdf(Module, ngsp:outputPattern, BNode),
    RDF::rdf(BNode, rdf:type, cnt, 'Chars'),  % type check
    RDF::rdf(BNode, ngsp:parameterName, literal(Type)), % of type string.
    RDF::rdf(BNode, cnt:chars, literal(Pattern)).

:- public(description/1).
description(Description):-
    ::ngs(RDF),
    ::second(Module),
    RDF::rdf(Module,dcterms:description,literal(Description)).

:- public(citation/1).
description(Citation):-
    ::ngs(RDF),
    ::second(Module),
    RDF::rdf(Module,schema:citation,literal(Citation)).

:- public(help/1).
help(Text):-
    ::ngs(RDF),
    ::second(Module),
    RDF::rdf(Module,schema:softwareHelp,literal(Text)).

:- public(web_site/1).
web_site(URL):-
    ::ngs(RDF),
    ::second(Module),
    RDF::rdf(Module,nco:websiteURL,URL).

:- public(category/1).
category(Name):-
    ::ngs(RDF),
    ::second(Module),
    RDF::rdf(Module,v:category,literal(Name)).

:- public(type_pattern/2).
type_pattern(Type, PatternString):-
    ::ngs(RDF),
    ::second(Module),
    RDF::rdf(Module, ngsp:outputPattern, GOP),
    RDF::rdf(GOP, rdf:type, cnt, 'Chars'),
    RDF::rdf(GOP, ngsp:pattern, Pattern),
    RDF::rdf(Pattern, dc:identifier, literal(Type)),
    RDF::rdf(Pattern, ngsp:patternString, literal(PatternString)).


% TODO: Proceed with command attributes public accessors.

:- end_object.

:- object(queryngs(_RDF),
          extends(ngsquerybase)).

:- protected(first/1).
first(RDF):-
    parameter(1, RDF).

:- public(module/3).
module(Module, Name, querymodule(RDF,Module)):-
    ::ngs(RDF),
    % RDF::graph(Mothur),
    ::mothur(Mothur),
    RDF::rdf(Mothur, ngsp:module, Module),
    RDF::rdf(Module, dc:title, literal(Name)).

:- public(mothur/1).
mothur(RES):-
    ::ngs(RDF),
    RDF::rdf(RES, rdf:type, ngsp, 'Specification').

:- end_object.
