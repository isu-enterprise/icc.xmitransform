
% Test transformation implementation.

:- use_module([library(semweb/rdf_prefixes)]).

:- object(tests, extends(lgtunit)).
:- info([
               version is 0.1,
               author is 'Evgeny Cherkashin',
               date is 2017/01/15,
               comment is 'Unit test for XMI tranforming utility'
           ]).

succeeds(test_test) :-
    true.

:- end_object.
