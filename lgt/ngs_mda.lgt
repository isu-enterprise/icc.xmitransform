:- use_module([library(semweb/rdf_prefixes)]).

% :- initialization(logtalk_load(config)).

absolute_file_name0(File, RelativeDir, Result):-
    atomic_list_concat([RelativeDir, File], '/', String),
    (
        %absolute_file_name(String,Result,[]) -> true;
        %format('WARNING: FIle Object ~p semms not to exist.\n',[String]),
        Result=String
    ).

:- object(mothur, instantiates(mothurrdfclass)).
:- end_object.

:- object(mda_scenario).

    :- public(run/0).
    run:-
        self(Self),
        this(This),
        writef:writef('Running transformation %w as instance of %w\n',[Self, This]),
        forall(::stage(Name, Goal),
               ::run(Name, Goal)).

    :- public(stage/2).

    :- use_module(writef, [swritef/3, writef/2, writef/1]).

    :- meta_predicate(run(*,0)).

    :- protected(run/2).
    run(Name, Goal):-
        writef('%w',[Name]),
        flush_output(user_output),
        (call(Goal)->
             writef(' succeeds\n',[Name]);
             writef(' failed\n',[Name])
        ).

    :- protected(warning/0).
    warning:-
        writef('This is a DUMMY scenario! Replace it with new stage/2 definitions!\n').

    :- protected(hello/0).
    hello:-
        writef('Hello World!\n').

    :- protected(hello/1).
    hello(X):-
        X='World',
        writef('Hello "%w"!\n',[X]).

    stage('Warning', ::warning).
    stage('Hello/0', ::hello).
    stage('Hello/1', ::hello(_)).


:- end_object.


%%%% NGS MDA Setup %%%%

:- object(ngs_config,
          extends(config)).

    :- protected(setup/0).
    setup:-
        ^^setup,
        ::option('ngs-project-dir'='/home/eugeneai/projects/code/NGS/rapidminer-extension'),
        ::option('ngs-java-modules-subdir'='src/main/java/com/rapidminer/ngs/operator'),
        ::option('ngs-resource-subdir'='src/main/resources/com/rapidminer/ngs/resources'),
        ::option('ngs-rdf-input-dir'='../tests/input/result.ttl'),
        expand,
        format('Set up the directories.\n'),
        true.

    :- uses(user, [absolute_file_name0/3]).
    :- private(expand/0).
    expand:-
        ::current_option('ngs-project-dir'=ProjectDir),
        format('Project DIR: ~p\n', [ProjectDir]),
        ::current_option('ngs-java-modules-subdir'=Java),
        ::current_option('ngs-resource-subdir'=Resource),
        absolute_file_name0(Java, ProjectDir, AJava),
        absolute_file_name0(Resource, ProjectDir, AResource),
        ::option('ngs-java-modules-dir'=AJava),
        format('Project Java DIR: ~p\n', [AJava]),
        ::option('ngs-resource-dir'=AResource),
        format('Project Resource DIR: ~p\n', [AResource]),
        absolute_file_name0('i18n', AResource, ADoc),
        ::option('ngs-resource-doc-dir'=ADoc),
        format('Project Resource doc DIR: ~p\n', [ADoc]).


    :- initialization(setup).

:- end_object.

%%%% NGS MDA Scenraio %%%%

:- object(ngs_mda,
          extends(mda_scenario)).

    :- protected(current_config/1).
    current_config(Setup):-
        root::current_config(Setup).

    :- protected(setup/1).
    setup(initialize):-
        mothur::set_graph(mothur),
        mothur::register_prefixes.

    setup(config):-
        root::config(ngs_config).

    :- protected(loadttl/0).
    loadttl:-
        ngs_config::current_option('ngs-rdf-input-dir'=Dir),
        mothur::load_file(Dir).

    :- private(psm_module/1).
    psm_module(Module):-
        mothurpsm(mothur)::module(Module).

    :- use_module(writef, [swritef/3, writef/2]).
    :- use_module(lists, [length/2]).

    :- protected(mda/1).
    mda(java_modules):-
        PSM=mothurpsm(mothur),
        PSM::modules,
        findall(Module,
                ::psm_module(Module),
                Modules),
        length(Modules, Length),
        writef('\nGenerated %w PSMs of Java modules',[Length]).

    mda(save_modules):-
        ::current_option('ngs-java-modules-dir'=JavaDir),
        mothurpsm(mothur)::render_modules_to_dir(JavaDir).

    mda(xml_operators):-
        forall(
            ::psm_module(Module),
            mothur_xml_psm(mothur_operators_psm)::generate_operators(Module)).

    mda(xml_doc_operators):-
        forall(
            ::psm_module(Module),
            mothur_xml_psm(mothur_operators_doc_psm)::generate_doc_operators(Module)).

    mda(save_xmls):-
        ::current_option('ngs-resource-dir'=Resource),
        mothur_xml_psm(mothur_operators_psm)::render_to_dir(Resource),
        ::current_option('ngs-resource-doc-dir'=ResourceDoc),
        mothur_xml_psm(mothur_operators_doc_psm)::render_to_dir(ResourceDoc).

    :- protected(stage/2).
    stage('Initialize environment',    ::setup(initialize)).
    stage('Setup generation context',  ::setup(setup)).
    stage('Loading RDF (TTL) sources', ::loadttl).
    stage('Transforming to Java modules', ::mda(java_modules)).
    stage('Generating sources of Java modules into .java files', ::mda(save_modules)).
    stage('Generating Mothur Operators PSMs', ::mda(xml_operators)).
    stage('Generating Mothur DOC Operators PSMs', ::mda(xml_doc_operators)).
    stage('Saving XMLs',                        ::mda(save_xmls)).

    :- public(debug/0).
    debug:-
        ::mda(xml_doc_operators).

:- end_object.
