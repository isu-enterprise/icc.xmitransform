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

succeeds(test_atom_split):-
    profile::atom_prefix_split('xmi:XMI','xmi','XMI').

succeeds(simple_load_profile):-
    profile::load_file('../tests/input/LocalProfile.profile.xmi').

succeeds(profile_graph_name):-
    profile::graph('LocalProfile').

succeeds(simple_profile_query):-
    profile::dom([element(_,_,_)]).

fails(simple_package_query):-
    package::dom([element(_,_,_)]).

succeeds(simple_load_package):-
    package::load_file('../tests/input/xmitransofmtest.xmi'),
    package::dom([_]).

succeeds(clear_package):-
    package::clear.

fails(package_try_dom):-
    package::dom([_]).

test(package_load_again):-
    package::load_file('../tests/input/xmitransofmtest.xmi'),
    package::dom([_]).

test(package_NS_ok):-
    package::namespace(_, _).

test(package_locations_ok):-
    package::location(_, _).

test(package_NS_with_locations):-
    package::namespace(_, _, _).

test(package_graph_name):-
    package::graph('xmitransofmtest').

succeeds(package_prefixes_register):-
    package::register_prefixes.

succeeds(package_process_xmi):-
    package::process.

:- end_object.
