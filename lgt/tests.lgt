

:- begin_tests(lgt).
:- logtalk_load('test.lgt').

test(test_check):-
	true.

test(simple):-
	list::member(3,[1,3,4]).

:- end_tests(lgt).

:- run_tests.

:- halt.
