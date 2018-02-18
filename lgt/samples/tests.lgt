
% Test transformation implementation.

:- use_module([library(semweb/rdf_prefixes)]).

:- object(localprofile, instantiates(profileclass)).
:- initialization((
                         ::load_file('../../tests/input/LocalProfile.profile.xmi'),
                         ::register_prefixes,
                         ::process
                     )).
:- end_object.

:- object(codeprofile, instantiates(profileclass)).
:- initialization((
                         ::load_file('../../tests/input/Code.profile.xmi'),
                         ::register_prefixes,
                         ::process
                     )).
:- end_object.

:- object(apackage, instantiates(packageclass)).
:- initialization((
                         ::load_file('../../tests/input/Samples/samples.xmi'),
                         ::register_prefixes,
                         ::process
                     )).
:- end_object.

:- object(tests, extends(lgtunit)).
:- info([
               version is 0.1,
               author is 'Evgeny Cherkashin',
               date is 2017/01/15,
               comment is 'Unit test for tranforming subsystem'
           ]).

succeeds(test_test) :-
    true.

succeeds(setup_setup_globally):-
    root::setup(setup),
    setup::set(use_tabs, false),
    setup::set(tab_size, 4).

succeeds(direct_3_is_an_object):-
    current_object(direct(apackage, localprofile, codeprofile)).

succeeds(test_tr_1) :-
    direct(apackage, localprofile, codeprofile)::tr(class, person, _),
    current_object(person).

:- end_object.
