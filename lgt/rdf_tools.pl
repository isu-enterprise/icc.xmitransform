:- module(rdf_tools,
          [ expand_uri/3,               % :Alias, +URI
            expand_object/3,            % :Alias, ?URI
            atom_prefix_split/3,
            atom_prefix_split/4,
            rdf_global_id_/2,
            rdf_global_object_/2,
            rdf_save_turtle_/2,
            rdf_current_prefix_/2,
            rdf_register_prefix_/3,
            load_xml_/3,
            rdf_/4,
            proc_ent/3,
            rdf_save_/2
          ]).

:- use_module(library(semweb/rdf_prefixes)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(library(sgml)).

rdf_global_id_(NS:Term,Global):-
    rdf_global_id(NS:Term,Global).
rdf_global_object_(NS:Term,Global):-
    rdf_global_object(NS:Term,Global).
rdf_(Subject,Predicate,Object,Graph):-
    % format("\nS>~w P>~w O>~w\n",[Subject,Predicate,Object]),
    proc_ent(n,Subject,Subject1),
    proc_ent(n,Predicate,Predicate1),
    proc_ent(o,Object,Object1),
    % format("\nS1>~w P1>~w O1>~w\n",[Subject1,Predicate1,Object1]),
    rdf(Subject1,Predicate1,Object1,Graph).
    % format("\nSR>~w PR>~w OR>~w\n",[Subject1,Predicate1,Object1]).

proc_ent(_,E,E):-
    var(E),!.
proc_ent(_,E,E):-
    atomic(E),!.
proc_ent(n,A:B,E):-
    nonvar(A),
    nonvar(B),
    rdf_global_id(A:B,E),!.
proc_ent(o,A:B,E):-
    nonvar(A),
    nonvar(B),
    rdf_global_object(A:B,E),!.
proc_ent(_,literal(V),literal(V)):-!.
proc_ent(_,A:B,_):-
    (var(A)->throw(variable(A,'Free variable as NS'));true),
    (var(B)->throw(variable(A,'Free variable as resource part'));true).
proc_ent(_,A,A):-
    format('\n proc_ent ? ~w\n',[A]).


rdf_save_(A,B):-rdf_save(A,B).
rdf_save_turtle_(A,B):-rdf_save_turtle(A,B).
rdf_register_prefix_(NS, URI, Opts):-
    rdf_register_prefix(NS, URI, Opts).

rdf_current_prefix_(Prefix, IRI):-
    rdf_current_prefix(Prefix, IRI).

load_xml_(A,B,C):-
    load_xml(A,B,C).

atom_prefix_split(Prefix:Suffix, Prefix, Suffix):-!.

atom_prefix_split(Atom, Prefix, Suffix):-!,
    atom_prefix_split(Atom, Prefix, ':', Suffix).

atom_prefix_split(Prefix:Suffix, Prefix, ':', Suffix):-!.

atom_prefix_split(Atom, Prefix, Divider, Suffix):-
    atom_length(Divider, DividerLength),
    sub_atom(Atom, B, DividerLength, A, Divider),
    sub_atom(Atom, 0, B, _, Prefix),
    B1 is B+DividerLength,
    sub_atom(Atom, B1,A, 0, Suffix).

expand_uri(nil, _, _Graph):-!, fail.

expand_uri(href(URI), URI, _Graph):-!.

expand_uri(NS:O, EObject, _Graph):-!,
    rdf_current_prefix(NS,_URI), !,
    rdf_global_id(NS:O, EObject).

expand_uri(URI, URI, _Graph):-
    atom_prefix_split(URI, Protocol, "://",_Id),
    member(Protocol, ['http','https','ftp','file']),!.

expand_uri(Object, EObject, _Graph):-
    atom_prefix_split(Object, NS, O),
    rdf_current_prefix(NS,_URI), !,
    rdf_global_id(NS:O, EObject).

expand_uri(Object, EObject, Graph):-
    rdf_global_id(Graph:Object, EObject).

expand_object(href(A), A, _Graph):-!.
expand_object(Object, URI, Graph):-
    atom_prefix_split(Object, '','_',_Rest),!,
    expand_uri(Object, URI, Graph).
expand_object(Object, URI, Graph):-
    atom_prefix_split(Object, _Prefix,":",_Suffix),!,
    expand_uri(Object, URI, Graph).
expand_object(Atom, literal(Atom), _Graph).
%% expand_object(Object, EObject):-
%%     ::expand_uri(Object, EObject).
