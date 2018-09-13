:- use_module([library(semweb/rdf_prefixes)]).


:- object(rdfprivclass, specializes(xmiclass)).
:- public(test_atom_split/3).
test_atom_split(A,B,C):-
    ::atom_prefix_split(A,B,C).
:- end_object.

:- object(rdfpriv, instantiates(rdfprivclass)).
:- end_object.

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
    rdfpriv::test_atom_split('xmi:XMI','xmi','XMI').

succeeds(test_uri_normalise_1):-
    local_profile::uri_normalize('http://irnok.net/ontology/1.0', 'http://irnok.net/ontology/1.0#').

succeeds(test_uri_normalise_2):-
    local_profile::uri_normalize('http://irnok.net/ontology/1.0/', 'http://irnok.net/ontology/1.0/').

succeeds(test_uri_normalise_3):-
    local_profile::uri_normalize('http://irnok.net/ontology/1.0#', 'http://irnok.net/ontology/1.0#').

succeeds(simple_load_profile):-
    local_profile::load_file('../tests/input/LocalProfile.profile.xmi').

succeeds(profile_graph_name):-
    local_profile::graph('LocalProfile').

succeeds(simple_profile_query):-
    local_profile::dom([element(_,_,_)]).

fails(simple_package_query):-
    package::dom([element(_,_,_)]).

succeeds(simple_load_package):-
    package::load_file('../tests/input/XMITransformTest.xmi'),
    package::dom([_]).

succeeds(clear_package):-
    package::clear.

fails(package_try_dom):-
    package::dom([_]).

test(package_load_again):-
    package::load_file('../tests/input/XMITransformTest.xmi'),
    package::dom([_]).

test(package_NS_ok):-
    package::namespace('uml', _).

test(package_locations_ok):-
    package::location(_, _).

test(package_NS_with_locations):-
    package::namespace(_, _, _).

test(package_graph_name):-
    package::graph('XMITransformTest').

succeeds(package_prefixes_register):-
    package::register_prefixes,
    rdf_db::rdf_current_prefix('uml',_).

succeeds(package_process_xmi):-
    package::process.

succeeds(package_save_turtle):-
    package::save_turtle('../tests/output/XMITransformTest.ttl').

succeeds(code_profile_load_and_export):-
    code_profile::load_file('../tests/input/Code.profile.xmi'),
    code_profile::process,
    code_profile::save_turtle('../tests/output/Code.profile.ttl').

succeeds(local_profile_export):-
    local_profile::process,
    local_profile::save_turtle('../tests/output/LocalProfile.profile.ttl').

succeeds(save_all_as_rdf):-
    package::save_rdf('../tests/output/XMITransformTest.rdf'),
    code_profile::save_rdf('../tests/output/Code.profile.rdf'),
    local_profile::save_rdf('../tests/output/LocalProfile.profile.rdf').

succeeds(qurey_class_1):-
    query(package)::class('Document',_).

succeeds(qurey_class_2):-
    query(package)::class('Document',ClassID),
    query(package)::attribute('number', ClassID,_).

succeeds(qurey_class_methods_1):-
    query(package)::class('Document',ClassID),
    findall(Name, query(package)::method(Name, ClassID,_), L),
    L==['accept','revoke'].


:- end_object.
