

:- use_module(library(sgml)).

:- object(metaclass, instantiates(metaclass)).
:- end_object.

:- object(class, instantiates(metaclass)).
:- end_object.

:- object(xmiclass,
      specializes(class)).
   :- public([load_file/1, load_file/2, dom/1]).
   :- private([dom_/1]).
   :- dynamic([dom_/1]).

   load_file(FileName):-
		load_file(FileName, []).

   load_file(FileName, Options):-
		open(FileName, read, I),
		sgml::load_xml(I, DOM, Options),
		::assert(dom_(DOM)),
		close(I).

	dom(X) :-
			   ::dom_(X).

:- end_object.

:- object(xmi, instantiates(xmiclass)).
:- end_object.

:- object(packageclass, specializes(xmiclass)).
:- end_object.

:- object(package, instantiates(packageclass)).
:- end_object.

:- object(profileclass, specializes(xmiclass)).
:- end_object.

:- object(profile, instantiates(profileclass)).
:- end_object.
