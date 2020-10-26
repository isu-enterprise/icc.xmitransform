
:- use_module(library(lists)).
:- use_module(library(xpath)).
:- use_module(library(writef)).
:- use_module(library(option)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/turtle)).

:- use_module(rdf_tools,
                [ expand_uri/3,               % :Alias, +URI
                  expand_object/3,            % :Alias, ?URI
                  atom_prefix_split/3,
                  atom_prefix_split/4,
                  proc_ent/3,
                  rdf_save_turtle_/2,
                  rdf_register_prefix_/3,
                  load_xml_/3,
                  rdf_/4,
                  rdf_save_/2
                ]).

:- object(rdfmetaclass, instantiates(rdfmetaclass)).
:- end_object.

:- object(rdfclass, instantiates(rdfmetaclass)).
    :- public([
           clear/0,  %!
           location/2, %!
           namespace/2, %!
           namespace/3, %!
           add_namespace/2, %!
           add_location/2, %!
           set_graph/1, %!
           graph/1,  %!
           rdf/3, %!
           rdf/4, %!
           uri_normalize/2, %!
           save_turtle/1, %!
           save_turtle/2, %!
           save_rdf/1, %!
           save_rdf/2, %!
           register_prefixes/0 %!
       ]).
    :- private([
           location_/2, %!
           namespace_/2, %!
           graph_/1 %!
       ]).

    :- dynamic([
           namespace_/2,  % name -> URL
           location_/2,   % URL -> URL | FIle
           graph_/1       % The Graph name to store triples, defaults to name attribute of uml:Model attribute.
       ]).

    :- protected([
           rdf_assert/3, %!
           check_rdf_assert/3, %!
           uri_good_last_char/1, %!
           href_normalize/2, %!
           atom_starts_with/3, %!
           atom_last_char/2 %!
       ]).

    :- use_module(lists, [member/2]).
    :- use_module(rdf_tools, [proc_ent/3,rdf_save_/2,
                            rdf_register_prefix_/3,
                            rdf_save_turtle_/2, rdf_/4,
                            atom_prefix_split/4,
                            expand_uri/3,
                            expand_object/3,
                            atom_prefix_split/3
                            ]).
    :- use_module(rdf_db, [rdf_assert/4]).

    clear:-
        ::retractall(location_(_,_)),
        % FIXME: Unregister NSs?
        ::retractall(namespace_(_,_)).

    set_graph(X):-
        nonvar(X),!,
        ::retractall(graph_(X)),
        ::asserta(graph_(X)).

    graph(X):-
        ::graph_(X).


    check_rdf_assert(_,'__ignore__',_):-!.
    check_rdf_assert(Subject, Predicate, Object):-
        ::graph(Graph),
        expand_uri(Subject, ESubject, Graph),
        % format("\n PRED: ~w", [Predicate]),
        expand_uri(Predicate, EPredicate, Graph),
        expand_object(Object, EObject, Graph),
        !,
        ::rdf_assert(ESubject, EPredicate, EObject).
    check_rdf_assert(Subject, Predicate, Object):-
        format("\nBAD ASSERT: <~w,~w,~w>", [Subject, Predicate, Object]).

    rdf_assert(Subject, Predicate, Object):-
        ::graph(Graph),
        rdf_assert(Subject, Predicate, Object, Graph).

    rdf(Subject, Predicate, Object):-
        ::graph(Graph),
        %format("\n-RDF\n"),
        rdf_(Subject, Predicate, Object, Graph).
        %format("\n+RDF\n")

    rdf(Subject, Predicate, NS, Term):-
        proc_ent(o,NS:Term, ETerm),
        rdf(Subject, Predicate, ETerm).

    href_normalize(_Id, Id):-
        atom_prefix_split(_Id, Prefix, "#", Suffix),
        atom_concat(Prefix, "#", _Prefix),
        ::location(URI, _Prefix),
        ::namespace(NS, URI), !,
        atom_concat(NS,":", _NS),
        atom_concat(_NS, Suffix, Id).

    href_normalize(Id, href(Id)).

    atom_starts_with(Atom, Sub, Rest):-
        sub_atom(Atom, 0, L, A, Sub),
        sub_atom(Atom, L, A, 0, Rest).

    atom_last_char(Atom, Char):-
        atom_length(Atom, L),!,
        L1 is L - 1,!,
        sub_atom(Atom, L1, 1, 0, Char).


    save_turtle(Out, Options):-
        ::graph(Graph),
        rdf_save_turtle_(Out, [graph(Graph) | Options]).

    save_turtle(Out):-
        ::save_turtle(Out, [
                          a(true),
                          align_prefixes(true),
                          canonize_numbers(true),
                          indent(2),
                          group(true),
                          only_known_prefixes(true),
                          single_line_bnodes(true),
                          tab_distance(0),
                          user_prefixes(true)
                      ]).

    save_rdf(Out):-
        ::base_uri(URI),
        save_rdf(Out, [
                       base_uri(URI),
                       encoding(utf8)
                   ]).

    save_rdf(Out, Options):-
        ::graph(Graph),
        rdf_save_(Out, [graph(Graph) | Options]).

    register_prefixes:-
        ::namespace(NS, URI),
        rdf_register_prefix_(NS, URI, [keep(true)]),
        fail; true.

    add_namespace(NS, URI):-
        ::assertz(namespace_(NS, URI)).

    namespace(NS, URI):-
        ::namespace_(NS, URI).

    namespace(NS, URI, Location):-
        namespace(NS, URI),
        ::location(URI, Location).

    namespace(NS, URI, nil):-
        namespace(NS, URI),
        \+ ::location(URI, _).

    add_location(URI, Location):-
        ::assertz(location_(URI, Location)).

    location(URI, Location):-
        ::location_(URI, Location).

    uri_good_last_char('/').
    uri_good_last_char('#').

    uri_normalize(NS, NS):-
        atom_last_char(NS,C),
        ::uri_good_last_char(C),!.
    uri_normalize(NS, NewNs):-
        atom_concat(NS,'#',NewNs).

:- end_object.

:- object(xmiclass,
      specializes(rdfclass)).

    :- public([
            load_file/1,
            load_file/2,
            dom/1,
            clear/0, %!
            xpath/2,
            write/0,
            namespace/2, %!
            filename/1,
            process/0,
            base_uri/1
            ]).

    :- private([
            dom_/1,
            process_namespaces/0,
            process_ns_locations/0,
            top_name_to_graph/0,
            filename_/1
            ]).

    :- protected([
            xmlns/2,
            top_xmi_element/1,
            top_subject/1,
            process_attrs_def/4,
            process_attrs_rest/2,
            process_attr/3,
            process_attr_/6,
            find_attr_/4,
            process_elements/2,
            process/3,
            process_atom/4,
            base_check/1
            ]).

    :- dynamic([
            dom_/1,
            filename_/1,   % Name of file loaded. It corresponds to NS nil.
            top_subject/1 % References to a top subject of the Package/Profile.
            ]).

    load_file(FileName):-
        ::load_file(FileName, []).

    :- use_module(rdf_tools, [proc_ent/3,rdf_save_/2,
                            rdf_register_prefix_/3,
                            rdf_save_turtle_/2, rdf_/4,
                            atom_prefix_split/4,
                            load_xml_/3,
                            atom_prefix_split/3
                            ]).
    :- use_module(xpath, [xpath/3 as xpath_xpath/3]).

    load_file(FileName, Options):-
        open(FileName, read, I),!,
        load_xml_(I, DOM, Options),!,
        ::base_check(DOM),!,
        ::assertz(dom_(DOM)),!,
        ::assertz(filename_(FileName)),!,
        close(I),!,
        ::process_ns_locations,!,
        ::process_namespaces,!,
        ::top_name_to_graph.

    dom(X) :-
        ::dom_(X).

    filename(FileName):-
        ::filename_(FileName).

    clear:-
        ::retractall(dom_(_)),
        ::retractall(filename_(_)),
        ^^clear.

    xpath(Spec, Content):-
        ::dom(DOM),
        xpath_xpath(DOM, Spec,  Content).

    % ----------------- Main processing recursion -----------------------------------

    process:-
        ::dom([Root]),
        ::process(Root, _Relation, _OId).

    process(element(Atom, Attrs, Elements), Relation, OId):-
        % format("ELEMENT: ~w<~w>", [Atom, Attrs]),
        ::process_atom(Atom, Attrs, OId, Relation),
        ::process_elements(Elements, OId).

    process_elements([], _).
    process_elements([element(A,B,C)|T], SId):-!,
        ::process(element(A,B,C), Relation, OId),
        ::check_rdf_assert(SId, Relation, OId),
        process_elements(T, SId).
    process_elements([X|T], SId):-
        format("Text?:~w",[X]),
        process_elements(T, SId).

    process_atom('xmi:XMI',_,'XMIFileID','__ignore__'):-!.

    process_atom(Atom, Attrs, Id, Atom):-
        format("TRY:ATOM ~w ", [Atom]),
        atom_prefix_split(Atom, _P,_S),!,
        format("GOOD:ATOM ~w ", [Atom]),
        ::process_attrs_def(Attrs, Id, Atom, RestAttrs), !, % NOTE: Atom=Type is defined here.
        ::process_attrs_rest(Id, RestAttrs).

    process_atom(UML, Attrs, Id, uml:UML):- % Unprefixed tags implied to be uml:<tag>
        format("UML ~w ", [UML]),
        % (UML=='defaultValue'->debugger::trace;true),
        ::process_attrs_def(Attrs, Id, _Type, RestAttrs), !,
        ::process_attrs_rest(Id, RestAttrs).

    process_atom(Atom, Attrs, nil, nil):-
        format('FAILED PROCESS: ~w(~w)',[Atom, Attrs]).



    find_attr_(type, Attrs, Type, Attrs):-
        nonvar(Type),!.
    find_attr_(type, Attrs, Type, RestAttrs):-!,
        ::process_attr(Attrs, 'xmi:type'(Type), RestAttrs).
    find_attr_(name, Attrs, Name, RestAttrs):-!,
        ::process_attr(Attrs, name(Name), RestAttrs).
    find_attr_(id, Attrs, Id, RestAttrs):-
        ::process_attr(Attrs, 'xmi:id'(Id), RestAttrs),!.
    find_attr_(id, Attrs, Id, RestAttrs):-
        ::process_attr(Attrs, href(_Id), RestAttrs),!,
        ::href_normalize(_Id, Id).

    process_attr_(Kind, Id, Attrs, Subject, RestAttrs, Relation):-
        find_attr_(Kind, Attrs, Subject, RestAttrs),
        nonvar(Id),
        Id \= nil,
        nonvar(Relation),
        nonvar(Subject),!,
        ::check_rdf_assert(Id, Relation, Subject).

    process_attr_(_, _, Attrs, _, Attrs, _).

    process_attrs_def(Attrs, Id, Type, RestAttrs):-
        ::find_attr_(id, Attrs, Id, R1),
        format("DEF:Attrs:~w, for id ~w", [Attrs, Id]),
        ::process_attr_(type, Id, R1, Type, R2, 'rdf:type'),
        ::process_attr_(name, Id, R2, _Name, RestAttrs, 'rdfs:label').

    process_attrs_rest(_,[]).
    process_attrs_rest(Id, [A=B|T]):-
        Id\=nil,!,
        ::check_rdf_assert(Id, A, B),
        process_attrs_rest(Id, T).

    :- use_module(swi_option, [select_option/3,option/2 as swi_get_option/2]).

    process_attr(Attrs, Struct, RestAttrs):-
        select_option(Struct, Attrs, RestAttrs).

    % ----------------- END OF Main processing recursion ----------------------------


    :- private([
            p_ns/1,
            p_locs/1,
            p2_locs/1,
            p2_locs/2,
            p2_locs_/2
            ]).

    process_namespaces:-
        ::dom([element(_, Attrs, _)]),
        ::p_ns(Attrs).

    p_ns(Key=_Val):-
        % format("XMLNS: ~w -> ~w\n",[Key, _Val]),
        ::xmlns(Key, NS),!,
        ::uri_normalize(_Val, Val),
        ::add_namespace(NS, Val),
        format("Added xmlns ~w=~w\n",[NS, Val]).
    p_ns(_=_).
    p_ns([X]):-
        p_ns(X).
    p_ns([X|T]):-
        p_ns(X),
        p_ns(T).

    process_ns_locations:-
        ::dom([element(_,Attrs,_)]),
        swi_get_option('xsi:schemaLocation'(Val), Attrs),!,
        ::p_locs(Val).

    process_ns_locations.

    p_locs(Val):-
        % format("Loc:~w", [Val]),
        split_string(Val, ' ',' ', L),
        ::p2_locs(L).

    p2_locs_(Location, _Location):-
        atom_prefix_split(Location, Prefix, '.xmi#', _Suffix),!,
        atom_concat(Prefix,'.xmi#', _Location).
    p2_locs_(Location, Location).

    p2_locs(URI, Location):-
        atom_string(_URI, URI),
        atom_string(_Loc, Location),
        p2_locs_(_Loc, _Location),
        ::uri_normalize(_URI, _NormURI),
        ::add_location(_NormURI, _Location).
    p2_locs([]).
    p2_locs([URI,Location|R]):-p2_locs(URI,Location), p2_locs(R).

    base_uri(URI):-
        ::filename(FileName),
        atom_concat('file://', FileName, _S1),
        ::uri_normalize(_S1, URI).

    namespace(Graph, URI):-
        ^^namespace(Graph, URI).

    namespace(Graph, URI):-
        ::graph(Graph),!,
        ::base_uri(URI).

    xmlns(Key, NS):-
        atom_prefix_split(Key, 'xmlns', NS).

    top_name_to_graph.


    % Auxiliary predicates used for debugging.

    :- use_module(writef, [writef/2 as writef_writef/2]).

    write:-
        ::dom(DOM),
        writef_writef("\n-------------\n%w\n-------------\n", [DOM]).


    base_check([_]).

:- end_object.

:- object(xmi, instantiates(xmiclass)).
:- end_object.

:- object(packageclass, specializes(xmiclass)).
    top_xmi_element('uml:Model').

    base_check([element('xmi:XMI',_,_)]):-!.
    base_check([element(TopElement,_,_)]):-
        ::top_xmi_element(TopElement).

    :- use_module(swi_option, [select_option/3,option/2 as swi_get_option/2]).

    top_name_to_graph:-
        ::top_xmi_element(TopElement),
        % ::xpath(//'xmi:XMI'/TopElement, element(_,Attrs,_)),
        ::xpath(//TopElement, element(_,Attrs,_)),
        format("Mdel--> ~w",[Attrs]),
        swi_get_option(name(Name), Attrs),!,
        ::set_graph(Name).
    top_name_to_graph:-
        ^^top_name_to_graph.
:- end_object.

:- object(package, instantiates(packageclass)).
:- end_object.

:- object(profileclass, specializes(xmiclass)).
    :- use_module(swi_option, [select_option/3,option/2 as swi_get_option/2]).

    top_xmi_element('uml:Profile').
    base_check([element(TopElement,_,_)]):-
        ::top_xmi_element(TopElement).
    top_name_to_graph:-
        ::top_xmi_element(TopElement),
        ::xpath(//TopElement, element(_,Attrs,_)),
        format("Mdel--> ~w",[Attrs]),
        swi_get_option(name(Name), Attrs),!,
        ::set_graph(Name).
:- end_object.

:- object(local_profile, instantiates(profileclass)).
:- end_object.

:- object(code_profile, instantiates(profileclass)).
:- end_object.



% Mothur description as RDF graph saved as TTL

:- object(mothurrdfclass,
          specializes(rdfclass)).

    :- public(load_file/1).
    load_file(FileName):-
        ::load_file(FileName,[anon_prefix(bnode)]).

    :- use_module(lists, [member/2]).
    :- use_module(turtle, [rdf_read_turtle/3]).

    :- public(load_file/2).
    load_file(FileName, Options):-
        rdf_read_turtle(FileName, Triples, Options),
        forall(member(Triple, Triples), ::process_triple(Triple)).

    :- protected(process_triple/1).

    process_triple(rdf(S,P,O)):-
        ::rdf_assert(S,P,O).

    process_triple(rdf(S,O,P,G)):-
        (::graph(G)->true;::set_graph(G)),
        ::rdf_assert(S,O,P).

    :- public(namespace/2).

    namespace(Name, URI):-
        ^^namespace(Name, URI).

    namespace(cnt,'http://www.w3.org/2011/content#').
    namespace(mothur,'http://icc.ru/ontologies/NGS/mothur/').
    namespace(ngs,'http://icc.ru/ontologies/NGS/').
    namespace(ngsp,'http://icc.ru/ontologies/NGS/processing/').
    namespace(oslc,'http://open-services.net/ns/core#').
    namespace(rdfs,'http://www.w3.org/2000/01/rdf-schema#').
    namespace(schema,'http://schema.org/').
    namespace(v,'http://www.w3.org/2006/vcard/ns#').
    namespace(xml,'http://www.w3.org/XML/1998/namespace').
    namespace(nco,'http://www.semanticdesktop.org/ontologies/2007/03/22/nco#').

:- end_object.
