%:- use_module([library(semweb/rdf_prefixes)]).
:- use_module([library(writef)]).

:- object(test_block,
          specializes(code_block)).
:- end_object.


:- object(tinst, instantiates(code_block)).
:- end_object.


:- object(tests, extends(lgtunit)).
:- info([
               version is 0.1,
               author is 'Evgeny Cherkashin',
               date is 2017/01/22,
               comment is 'Unit test for a Python code generator.'
           ]).

succeeds(block_add_1) :-
    tinst::append(1).

succeeds(block_prepend) :-
    tinst::prepend(0).

succeeds(block_add_2) :-
    tinst::append(2).

succeeds(block_list):-
    findall(X, tinst::item(X), L),
    writef::writef(':::: %w',[L]),
    L == [0,1,2].

succeeds(test_clear) :-
    tinst::clear.

succeeds(block_list_2):-
    findall(X, tinst::item(X), []).

:- end_object.
