:- use_module([library(semweb/rdf_prefixes)]).

:- object(mda_scenario).

:- public(run/0).
run:-
    self(Self),
    writef:writef('Running transformation %w\n',[Self]),
    forall(::stage(Name, Goal),
           ::run(Name, Goal)).

:- public(stage/2).

:- protected(run/2).
run(Name, Goal):-
    writef:writef('Running stage "%w"',[Name]),
    flush_output(user_output),
    (call(Goal)->
         writef::writef(' succeeds\n',[Name]);
         writef::writef(' failed\n',[Name])
    ).

:- protected(warning/0).
warning:-
    writef:writef('This is a DUMMY scenario! Replace it with new stage/2 definitions!\n').

:- protected(hello/0).
hello:-
    writef:writef('Hello World!\n').

:- protected(hello/1).
hello(X):-
    X='World',
    writef:writef('Hello "%w"!\n',[X]).

stage('Warning', ::warning).
stage('Hello/0', ::hello).
stage('Hello/1', ::hello(_)).


:- end_object.



:- object(ngs_mda,
          extends(mda_scenario)).

:- protected(setup/1).
setup(initialize):-
    mothur::set_graph(mothur),
    mothur::register_prefixes.

setup(setup):-
    root::setup(setup).


:- protected(loadttl/0).
loadttl:-
    mothur::load_file('../tests/input/result.ttl').

:- protected(mda/1).
mda(java_modules):-
    PSM=mothurpsm(mothur),
    PSM::modules,
    findall(Module,
            PSM::module(Module),
            Modules),
    lists::length(Modules, Length),
    writef::writef('\nGenerated %w PSMs of Java modules',[Length]).

mda(save_modules):-
    mothurpsm(mothur)::render_modules_to_dir('/var/tmp').


:- protected(stage/2).
stage('Initialize environment',    ::setup(initialize)).
stage('Setup generation context',  ::setup(setup)).
stage('Loading RDF (TTL) sources', ::loadttl).
stage('Transforming to Java modules', ::mda(java_modules)).
stage('Saving Java modules into .java files', ::mda(save_modules)).

:- public(debug/0).
debug:-
    ::mda(save_modules).

:- end_object.


:- object(mothur, instantiates(mothurrdfclass)).
:- end_object.
