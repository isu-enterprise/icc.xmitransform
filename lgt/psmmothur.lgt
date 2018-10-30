:- use_module(library(pcre)).

camel_case_name0(Name, UpcasedName):-
    pcre:re_split("\\w+", Name, [""|T]),
    filter0(T,UT),
    concat_atom(UT,UpcasedName).

filter0([],[]).
filter0([X,_|T], [CX|UT]):-!,
    camel_case(X,CX),
    filter0(T,UT).

camel_case(Word,UWord):-
    text_to_string(Word, String),
    sub_string(String,0,1,A,Char),
    sub_string(String,1,A,0,Rest),
    string_upper(Char,UChar),
    string_concat(UChar,Rest,UWord).

:- object(psm(_RDF)).

:- info([
         comment is 'Generator of PSM from RDF graph subscenario.'
           ]).

:- protected(rdf/1).
rdf(RDF):-
    parameter(1,RDF).

:- end_object.

%%%%%%%%%%%%%%%% Mothur Java module PSM generator %%%%%%%%%%%%%%%%%%%%%%%%%555

:- object(java_modules,
          instantiates(code_block)).
:- end_object.

:- object(mothurpsm(_RDF),
         extends(psm(_RDF))).

:- public(queryngs/1).
queryngs(queryngs(RDF)):-
    ::rdf(RDF).

:- public(module/2).
module(_Module,M):-
    ::queryngs(Q),
    rdf_db::rdf_global_object(_Module,Module),
    Q::module(Module,Name,QM),
    create_object(M, [instantiates(mothur_module)],[],[]),
    M::preamble,
    M::set_query(Q),
    M::current_block(class(Class)),
    Class::set_reference(module(M)),
    ::mothur_class_name(Name,ModuleName),
    Class::name(ModuleName),
    forall(attribute(QM,Class),true),
    forall(method(QM,Class),true).

:- public(modules/0).
modules:-
    java_modules::clear,  % TODO: better destroy all objects.
    forall(::module(_,Module),
           java_modules::append(module(Module))).

:- public(render_modules_to_dir/1).
render_modules_to_dir(Directory):-
    forall(::module(Module),
           render_module_to_dir(Module,Directory)).

:- private(render_module_to_dir/2).
render_module_to_dir(Module, Directory):-
    Module::module_name(Name),
    absolute_file_name(Name, PathName,
                       [relative_to(Directory), expand(true)]),
    open(PathName,write,Stream,[]),
    Module::render_to(Stream),
    close(Stream).

:- public(module/1).
module(Module):-
    java_modules::item(module(Module)).

:- protected(attribute/2).
attribute(Query, Class):-
    Query::parameter(_Parameter, Name, QP),
    QP::type(mothur:'InputTypes'),
    Class::input_parameter(Name).

attribute(Query, Class):-
    Query::output_pattern_types(_, Type),
    Class::output_parameter(Type).

attribute(Query, Class):-
    Query::parameter(_Parameter, Name, QP),
    \+ QP::type(mothur:'InputTypes'),
    Class::property_parameter(Name, QP).

:- protected(method/2).
method(Query, Class):-
    self(Self),
    Class::method(mothur_get_output_pattern(Query,Class,Self)).

:- protected(mothur_class_name/2).
mothur_class_name(MothurModuleName, RMClassName):-
    camel_case_name(MothurModuleName, UpcasedName),
    writef::swritef(RMClassName,'Mothur%wOperator',[UpcasedName]).

:- uses(user, [camel_case_name0/2]).
:- protected(camel_case_name/2).
camel_case_name(Name,UpcasedName):-
    camel_case_name0(Name, UpcasedName).

:- public(type_pattern/3).
type_pattern(Type,Pattern,QM):-
    QM::type_pattern(Type,Pattern).

:- end_object.

%%%% XML PSM %%%%

:- object(mothur_operators_psm,
          instantiates(mothur_operators)).
:- initialization((::initialize_root)).
:- end_object.

:- object(mothur_operators_doc_psm,
          instantiates(mothur_operators_doc)).
:- initialization((::initialize_root)).
:- end_object.

%%%

:- object(mothur_xml_psm(_XML_PSM)).

:- public(dom/1).
dom(PSM):-
    parameter(1,PSM).

:- public(psm/1).
psm(DOM):-
    dom(DOM).

:- public(render_to_dir/1).
render_to_dir(Directory):-
    ::dom(PSM),
    PSM::render(DOM),
    PSM::file_name(Name),
    absolute_file_name(Name, PathName,
                       [relative_to(Directory), expand(true)]),
    open(PathName,write,Stream,[]),
    sgml_write::xml_write(Stream,DOM,[]),
    close(Stream).

:- public(generate_operators/1).
generate_operators(Module):-
    % format('\nGenerating xml for ~p',[Module]),
    ::dom(DOM),
    ::group(DOM, Block),
    Block::element(operator, OP),
    OP::element(key,Key),
    Module::module_key_name(MKey),
    Key::text(MKey),
    OP::element(class,Class),
    Module::module_xml_class_name(MClass),
    Class::text(MClass),
    OP::element(icon,Icon),
    Module::module_icon_name(MIcon),  % Viva StarControl II !!!
    Icon::text(MIcon).

:- public(module_group/1).
module_group([''-'','NGS_group'-'NGS','mothur_group'-'Mothur']).  % TODO: Stub.


:- protected(group/2).
group(DOM,Block):-
    DOM::current_reference(group(Block)),!.
group(DOM,Block):-
    ::module_group(Groups),
    ::groups0(DOM, Groups, Block),
    DOM::set_reference(group(Block)).


:- protected(group_doc/1).
group_doc(DOM):-
    DOM::current_reference(group_doc(_)),!.
group_doc(DOM):-
    ::module_group(Groups),
    ::groups1(DOM, Groups),
    format('------> Setting ref !!'),
    DOM::set_reference(group_doc(DOM)).

:- private(groups0/3).
groups0(Block,[],Block):-!.
groups0(Block,[Group-_ | T], B):-
    Block::element(group,[key=Group], B1),
    ::groups0(B1,T,B).


:- private(groups1/2).
groups1(DOM, Groups):-
    forall(lists::member(Group-Name, Groups),
            DOM::element(group,
                         [key=Group, name=Name], _)).

:- uses(user, [atomic_list_concat/3]).
:- public(generate_doc_operators/1).
generate_doc_operators(Module):-
    ::dom(DOM),
    ::group_doc(DOM),
    Module::module_class(Class,ClassName),
    DOM::element(operator, OP),
    OP::element(key,Key),
    Module::module_key_name(MKey),
    Key::text(MKey),
    OP::element(name,Name),
    atomic_list_concat(['',Text],'Mothur',ClassName),
    atomic_list_concat([DisplayName,''],'Operator',Text),
    Name::text(DisplayName),
    format('\nGenerating doc xml for ~p',[Module]).

:- end_object.
