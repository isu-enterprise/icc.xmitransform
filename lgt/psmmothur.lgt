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

:- object(mothurpsm(_RDF)).
:- info([
         comment is 'Generator of PSM from RDF graph subscenario.'
           ]).

:- protected(rdf/1).
rdf(RDF):-
    parameter(1,RDF).

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
