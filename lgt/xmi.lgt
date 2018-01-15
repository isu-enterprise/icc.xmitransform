

:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(writef)).

:- object(metaclass, instantiates(metaclass)).
:- end_object.

:- object(class, instantiates(metaclass)).
:- end_object.

:- object(xmiclass,
      specializes(class)).
:- public([
                 load_file/1,
	  	         load_file/2,
		         dom/1,
		         clear/0,
                 xpath/3,
                 write/0,
                 location/2,
                 namespace/2,
                 namespace/3
		     ]).
:- private([dom_/1,
            process_namespaces/0,
            process_ns_locations/0,
            location_/2,
            namespace_/2,
            debug/0,
            debug/1
           ]).

:- protected([
                    debugf/2,
                    debugf/3,
                    writef/2,
                    base_check/1,
                    xmlns/2
                ]).

:- dynamic([
                  dom_/1,
                  namespace_/2,  % name -> URL
                  location_/2,   % URL -> URL | FIle
                  debug/0,       % debug at all.
                  debug/1        % debug(<what>), e.g. debug(basic_checks).
              ]).

debug.
%debug(xmi_headers).
debug(xmlns).

load_file(FileName):-
	load_file(FileName, []).

load_file(FileName, Options):-
	open(FileName, read, I),
	sgml::load_xml(I, DOM, Options),
    ::base_check(DOM),
	::assert(dom_(DOM)),
	close(I),
    process_ns_locations,
    process_namespaces.

dom(X) :-
	::dom_(X).

clear:-
	::retractall(dom_(_)),
    ::retractall(location_(_,_)),
    ::retractall(namespace_(_,_)).

xpath(Spec, Content):-
    ::dom(DOM),
    xpath::xpath(DOM, Spec,  Content).

process_namespaces:-
    ::dom([element(Root, Attrs, _)]),
    ::debugf(xmi_headers,"Root:%w",[Root]),
    p_ns(Attrs).

p_ns(Key=Val):-
    ::writef("%w -> %w\n",[Key, Val]),
    xmlns(Key, NS),!,
    ::assert(namespace_(NS, Val)),
    ::debugf(xmlns, "Added %w=%w",[NS, Val]).
p_ns(_=_).
p_ns([X]):-
    p_ns(X).
p_ns([X|T]):-p_ns(X),p_ns(T).

process_ns_locations:-
    true.

namespace(NS, URI):-
    ::namespace_(NS, URI).

namespace(NS, URI, Location):-
    ::namespace(NS, URI),
    ::location(URI, Location).

namespace(NS, URI, nil):-
    ::namespace(NS, URI),
    \+ ::location(URI, _).

location(URI, Location):-
    ::location_(URI, Location).


xmlns(Key, NS):-
    sub_atom(Key, B, 1, A, ':'),
    sub_atom(Key, 0, B, _, 'xmlns'),
    B1 is B+1,
    sub_atom(Key,B1,A,0,NS).


% Auxiliary predicates used for debugging.

write:-
    ::dom(DOM),
    writef::writef("\n-------------\n%w\n-------------\n", [DOM]).

writef(String, List):-
    writef::writef(String, List).

debugf(String, List):-
    ::debug,!,
    ::writef('DEBUG:',[]),
    ::writef(String, List),
    nl.

debugf(_,_).

debugf(What, String, List):-
    ::debug(What),!,
    ::debugf(String, List).

debugf(_,_,_).


base_check([_]).

:- end_object.

:- object(xmi, instantiates(xmiclass)).
:- end_object.

:- object(packageclass, specializes(xmiclass)).
base_check([element('xmi:XMI',_,_)]).
:- end_object.

:- object(package, instantiates(packageclass)).
:- end_object.

:- object(profileclass, specializes(xmiclass)).
base_check([element('uml:Profile',_,_)]).
:- end_object.

:- object(profile, instantiates(profileclass)).
:- end_object.
