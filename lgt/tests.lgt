:- use_module([library(semweb/rdf_prefixes)]).
:- use_module(rdf_tools, [rdf_current_prefix_/2,atom_prefix_split/3]).

:- object(rdfprivclass, specializes(xmiclass)).
    :- public(test_atom_split/3).
    :- use_module(rdf_tools,[atom_prefix_split/3]).
    test_atom_split(A,B,C):-
        atom_prefix_split(A,B,C).
:- end_object.

:- object(rdfpriv, instantiates(rdfprivclass)).
:- end_object.

:- object(xml_root, instantiates(xml_block)).
    setup:-
        ::name(root),
        ::attributes([a=b,c=2]),
        ::text('Hello World').

    :- initialization(setup).
:- end_object.


:- object(tests, extends(lgtunit)).
    :- info([
                   version is 0:2:0,
                   author is 'Evgeny Cherkashin',
                   date is 2017-01-15,
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
        package::namespace('uml', IRI),
        format('IRI = ~w', [IRI]).

    test(package_locations_ok):-
        package::location(_, _).

    test(package_NS_with_locations):-
        package::namespace(_, _, _).

    test(package_graph_name):-
        package::graph('XMITransformTest').

    :- use_module(rdf_tools, [rdf_current_prefix_/2]).

    succeeds(package_prefixes_register):-
        package::register_prefixes,
        rdf_current_prefix_('uml',_).

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

    succeeds(query_class_1):-
        query(package)::class('Document',_).

    succeeds(query_class_2):-
        query(package)::class('Document',ClassID),
        query(package)::attribute('number', ClassID,_).

    succeeds(query_class_methods_1):-
        query(package)::class('Document',ClassID),
        findall(Name, query(package)::method(Name, ClassID,_), L),
        L==['accept','revoke'].

:- end_object.


    % Tests of RDF of mothur description

:- object(mothur, instantiates(mothurrdfclass)).
:- end_object.


:- object(mothurrdftests, extends(lgtunit)).
    :- info([
                   version is 0:2:0,
                   author is 'Evgeny Cherkashin',
                   date is 2018-09-14,
                   comment is 'Unit test for RDF imports of Mothur tool'
               ]).

    succeeds(test_test) :-
        true.

    succeeds(test_set_graph_name):-
        mothur::set_graph(mothur).

    succeeds(test_install_namespaces):-
        mothur::register_prefixes.

    succeeds(test_load_file) :-
        mothur::load_file('../tests/input/result.ttl').

    succeeds(test_save_graph):-
        mothur::save_turtle('../tests/output/mothur.ttl').

    :- use_module(lists, [length/2]).

    succeeds(test_request_modules_1):-
        findall(Name,
                queryngs(mothur)::module(_, Name, _),
                Answer),
        length(Answer, N),
        N>10.

    succeeds(test_set_etup):-
        root::config(config).

    succeeds(test_query_module_by_name):-
        queryngs(mothur)::module(_, 'chimera.ccode',_).

    succeeds(test_query_module_params_by_name):-
        queryngs(mothur)::module(_, 'chimera.ccode',QM),
        findall(ParameterName,
                QM::parameter(_, ParameterName, _),
                Names),
        length(Names, N),
        % writef::writef('Names: %w', [Names]),
        N >= 9.

    succeeds(test_query_module_parameter_descr):-
        Q=queryngs(mothur),
        Q::module(_,'chimera.ccode',QM),
        QM::parameter(_, 'fasta', QP),
        QP::name('fasta'),
        QP::type('http://icc.ru/ontologies/NGS/mothur/InputTypes'),
        QP::important.

    succeeds(test_query_module_descriptions):-
        Q=queryngs(mothur),
        Q::module(_,'chimera.ccode',QM),
        %    QM::parameter(_, 'fasta', QP),
        QM::output_pattern(_,_),
        %% writef::writef('Output pattern - type: %w\n----------\n%w\n-----------\n',
        %%                [Type,Pattern]),
        QM::help(_),
        %% writef::writef('Help:\n----------\n%w\n-----------\n',
        %%                [Text]),
        QM::web_site(_),
        QM::category('Sequence Processing').

    succeeds(test_query_module_output_pattern_list):-
        Q=queryngs(mothur),
        Q::module(_,'chimera.ccode',QM),
        findall(Type-PatternString,
                QM::type_pattern(Type, PatternString),
                Patterns),
        length(Patterns, 3).

    succeeds(test_default_mothur_module_render):-
        create_object(M, [instantiates(mothur_module)],[],[]),
        M::preamble,
        M::current_block(class(ClassDef)),
        ClassDef::name('MothurTestingOperator'),
        M::render_to(nil).

    succeeds(test__mothur_align_seqs_module_render):-
        Tr=mothurpsm(mothur),
        Tr::module(mothur:'align.seqs',Module),
        Module::render_to(1).

    succeeds(test__mothur_cc_module_render):-
        Tr=mothurpsm(mothur),
        Tr::module(mothur:'chimera.ccode',Module),
        Module::render_to(1).

    succeeds(test__mothur_collsingle_module_render):-
        Tr=mothurpsm(mothur),
        Tr::module(mothur:'collect.single',Module),
        Module::render_to(1).

    %% succeeds(test__modules_render_all):-
    %%     Tr=mothurpsm(mothur),
    %%     forall(Tr::module(_ID,Module),
    %%            Module::render_to(1)).

    succeeds(test_mothur_psm_synthesis_all_classes):-
        Tr=mothurpsm(mothur),
        findall(Res,
                Tr::module(Res,_Module),
                Classes),
        lists::length(Classes, N),
        N>10.

    succeeds(test_simple_xml_renderer):-
        xml_root::render(DOM),
        DOM=element(root,[a=b,c=2],['Hello World']).

:- end_object.
