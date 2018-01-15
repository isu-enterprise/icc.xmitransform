:- object(tests, extends(lgtunit)).
   :- info([
   	  version is 0.1,
	  	  author is 'Evgeny Cherkashin',
		  date is 2017/01/15,
		  comment is 'Unit test for XMI tranforming utility'
   ]).

succeeds(test_test) :-
    true.

succeeds(test_list) :-
    findall(X, listtest::member(X,[1,3,4]), L),
    L == [1,3,4].

succeeds(simple_load_profile):-
	profile::load_file('../tests/input/LocalProfile.profile.xmi').

succeeds(simple_profile_query):-
	profile::dom([element(_,_,_)]).

fails(simple_package_query):-
	package::dom([element(_,_,_)]).

succeeds(simple_load_package):-
	package::load_file('../tests/input/xmitransofmtest.xmi'),
	package::dom([_]).

:- end_object.
