:- module(rdf_exp,
          [ expand_uri/2,               % :Alias, +URI
            expand_object/2,            % :Alias, ?URI
            atom_prefix_split/3,
            rdf_global_id_/2,
            rdf_save_turtle_/2,
            rdf_register_prefix_/3,
            load_xml_/3,
            rdf_save_/2
          ]).

:- use_module(library(semweb/rdf_prefixes)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(library(sgml)).

rdf_global_id_(NS:Term,Global):-
    rdf_global_id(NS:Term,Global).

rdf_save_(A,B):-rdf_save(A,B).
rdf_save_turtle_(A,B):-rdf_save_turtle(A,B).
rdf_register_prefix_(NS, URI, Opts):-
    rdf_register_prefix(NS, URI, Opts).

load_xml_(A,B,C):-
    load_xml(A,B,C).

    atom_prefix_split(Atom, Prefix, Suffix):-
        atom_prefix_split(Atom, Prefix, ':', Suffix).

    atom_prefix_split(Atom, Prefix, Divider, Suffix):-
        atom_length(Divider, DividerLength),
        sub_atom(Atom, B, DividerLength, A, Divider),
        sub_atom(Atom, 0, B, _, Prefix),
        B1 is B+DividerLength,
        sub_atom(Atom, B1,A, 0, Suffix).

    expand_uri(nil, _):-!, fail.

    expand_uri(href(URI), URI):-!.

    expand_uri(URI, URI):-
        atom_prefix_split(URI, Protocol, "://",_Id),
        member(Protocol, ['http','https','ftp','file']),!.

    expand_uri(Object, EObject):-
        atom_prefix_split(Object, NS, O),
        rdf_current_prefix(NS,_URI), !,
        rdf_global_id(NS:O, EObject).
    expand_uri(Object, EObject):-
        graph(NS),
        rdf_global_id(NS:Object, EObject).

    expand_object(href(A), A):-!.
    expand_object(Object, URI):-
        atom_prefix_split(Object, '','_',_Rest),!,
        expand_uri(Object, URI).
    expand_object(Object, URI):-
        atom_prefix_split(Object, _Prefix,":",_Suffix),!,
        expand_uri(Object, URI).
    expand_object(Atom, literal(Atom)).
    %% expand_object(Object, EObject):-
    %%     ::expand_uri(Object, EObject).
