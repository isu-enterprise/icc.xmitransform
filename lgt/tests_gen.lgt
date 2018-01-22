:- use_module([library(semweb/rdf_prefixes)]).

:- object(tests, extends(lgtunit)).
:- info([
               version is 0.1,
               author is 'Evgeny Cherkashin',
               date is 2017/01/22,
               comment is 'Unit test for Python '
           ]).

succeeds(test_test) :-
    true.

:- end_object.
