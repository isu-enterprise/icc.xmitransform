:- use_module([library(semweb/rdf_prefixes)]).

:- object(test_block,
          specializes(code_block)).
:- end_object.


:- object(tinst, instantiates(test_block)).
:- end_object.


:- object(tests, extends(lgtunit)).
:- info([
               version is 0.1,
               author is 'Evgeny Cherkashin',
               date is 2017/01/22,
               comment is 'Unit test for Python '
           ]).

succeeds(test_test) :-
    tinst::clear.

:- end_object.
