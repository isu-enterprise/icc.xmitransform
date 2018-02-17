
% Test transformation implementation.

:- use_module([library(semweb/rdf_prefixes)]).

:- object(localprofile, instantiates(profileclass)).
:- initialization((::load_file('../../tests/input/LocalProfile.profile.xmi'))).
:- end_object.

:- object(codeprofile, instantiates(profileclass)).
:- initialization((::load_file('../../tests/input/Code.profile.xmi'))).
:- end_object.

:- object(apackage, instantiates(packageclass)).
:- initialization((::load_file('../../tests/input/Samples/samples.xmi'))).
:- end_object.

xmi_model(apackage, localprofile, codeprofile).

:- object(tests, extends(lgtunit)).
:- info([
               version is 0.1,
               author is 'Evgeny Cherkashin',
               date is 2017/01/15,
               comment is 'Unit test for tranforming subsystem'
           ]).

succeeds(test_test) :-
    true.

:- end_object.
