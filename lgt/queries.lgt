
% Query module

:- use_module(rdf_tools,
                [ atom_prefix_split/3,
                  proc_ent/3,
                  rdf_save_turtle_/2,
                  rdf_register_prefix_/3,
                  load_xml_/3,
                  rdf_save_/2
                ]).

:- object(query(_XMI_)).

    :- protected(xmi/1).
    :- public([class/2, attribute/3, method/3]).

    class(Name, ID):-
        _XMI_::rdf(ID,rdf:type,uml,'Class'),
        _XMI_::rdf(ID,rdfs:label, literal(Name)).

    attribute(Name, ClassID, ID):-
        _XMI_::rdf(ClassID, uml:ownedAttribute, ID),
        % _XMI_::rdf(ID, rdf:type, uml,'Property'), % this can be a type
        _XMI_::rdf(ID, rdfs:label, literal(Name)).

    method(Name, ClassID, ID):-
        _XMI_::rdf(ClassID, uml:ownedOperation, ID),
        _XMI_::rdf(ID, rdfs:label, literal(Name)).

:- end_object.


%%%%%%%%% Querying NGS RDF Graphs for modules. %%%%%%%%%%%%

:- object(ngsquerybase).

    :- protected(ngs/1).
    ngs(RDF):-
        ::first(RDF).

:- end_object.


%%%% Query Parameter parameters (features) %%%%

:- object(queryparam(_RDF_,_Parameter_),
          extends(ngsquerybase)).

    :- protected(first/1).
    first(_RDF_).

    :- public(type/1).

    type(Type) :-
        ::attr(type, Type).

    :- public(name/1).
    name(Name) :-
        ::attr(dc:title, literal(Name)).

    :- public(options/1).
    options(BNode):-
        ::attr(options, BNode).

    :- public(options/2).
    %% options(BNode, Values):-
    %%     ::options(BNode),
    %%     ::ngs(RDF),
    %%     ::second(Parameter),
    %%     findall(Value,
    %%             (RDF::rdf(BNode, dc:identifier, literal(Value))),
    %%             Values
    %%            ).

    options(BNode, Values):-
        ::options(BNode), % NOTE: Here it is irrelevant, but let it be
        ::attr(optionsOrig, literal(String)),!,
        atomic_list_concat(Values,'-',String).

    :- public(options_default/1).
    options_default(Value):-
        ::attr(optionsDefault, literal(Value)).

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

    :- public(sku/1).
    sku(Number):-
        ::attr(schema:sku, literal(type(_,String))),
        atom_number(String,Number).

    :- use_module(rdf_tools, [proc_ent/3,
                              rdf_register_prefix_/3,
                              rdf_save_turtle_/2]).

    :- use_module(writef, [writef/2]).

    :- protected(attr/2).
    attr(NS:Name, Value):-
        proc_ent(o,Value, V),
        _RDF_::rdf(_Parameter_, NS:Name, V).

    attr(Name, Value):-
        Name \= _:_,!,
        proc_ent(n,Value, V),
        _RDF_::rdf(_Parameter_, ngsp:Name, V).

    :- protected(bool_attr/1).
    bool_attr(Name):-
        ::attr(Name, literal(type('http://www.w3.org/2001/XMLSchema#boolean',true))),!.
    bool_attr(Name):-
        ::attr(Name, literal(type('http://www.w3.org/2001/XMLSchema#boolean',false))),!,
        fail.
    bool_attr(Name):-
        ::attr(Name, Value),
         writef('WARNING: attr %w of %w has wring bool value: %w (assuming false)\n',
                       [Name, _Parameter_, Value]),!,
        fail.

:- end_object.


%%%% Query Module %%%%

:- object(querymodule(_RDF_,_Module_),
          extends(ngsquerybase)).

    :- public(parameter/3).
    parameter(Parameter, ParameterName, queryparam(_RDF_,Parameter)):-
        _RDF_::rdf(_Module_, ngsp:parameter, Parameter),
        _RDF_::rdf(Parameter, rdf:type, ngsp, 'Parameter'),
        _RDF_::rdf(Parameter, dc:title, literal(ParameterName)).

    :- public(output_pattern/2).
    output_pattern(Pattern,Type):-
        _RDF_::rdf(_Module_, ngsp:outputPattern, BNode),
        _RDF_::rdf(BNode, rdf:type, cnt, 'Chars'),  % type check
        _RDF_::rdf(BNode, ngsp:parameterName, literal(Type)), % of type string.
        _RDF_::rdf(BNode, cnt:chars, literal(Pattern)).

    :- public(output_pattern_types/2).
    output_pattern_types(Pattern,Identifier):-
        _RDF_::rdf(_Module_, ngsp:outputPattern, BNode),
        _RDF_::rdf(BNode, ngsp:pattern, BList),
        _RDF_::rdf(BList, ngsp:patternString, literal(Pattern)),
        _RDF_::rdf(BList, dc:identifier, literal(Identifier)).

    :- public(description/1).
    description(Description):-
        _RDF_::rdf(_Module_,dcterms:description,literal(Description)).

    :- public(citation/1).
    description(Citation):-
        _RDF_::rdf(_Module_,schema:citation,literal(Citation)).

    :- public(help/1).
    help(Text):-
        _RDF_::rdf(_Module_,schema:softwareHelp,literal(Text)).

    :- public(web_site/1).
    web_site(URL):-
        _RDF_::rdf(_Module_,nco:websiteURL,URL).

    :- public(category/1).
    category(Name):-
        _RDF_::rdf(_Module_,v:category,literal(Name)).

    :- public(type_pattern/2).
    type_pattern(Type, PatternString):-
        _RDF_::rdf(_Module_, ngsp:outputPattern, GOP),
        _RDF_::rdf(GOP, rdf:type, cnt, 'Chars'),
        _RDF_::rdf(GOP, ngsp:pattern, Pattern),
        _RDF_::rdf(Pattern, dc:identifier, literal(Type)),
        _RDF_::rdf(Pattern, ngsp:patternString, literal(PatternString)).


    % TODO: Proceed with command attributes public accessors.

:- end_object.

:- object(queryngs(_RDF_),
          extends(ngsquerybase)).

    :- public(module/3).
    module(Module, Name, querymodule(_RDF_,Module)):-
        % RDF::graph(Mothur),
        ::mothur(Mothur),
        _RDF_::rdf(Mothur, ngsp:module, Module),
        _RDF_::rdf(Module, dc:title, literal(Name)).

    :- public(mothur/1).
    mothur(RES):-
        _RDF_::rdf(RES, rdf:type, ngsp, 'Specification').

:- end_object.
