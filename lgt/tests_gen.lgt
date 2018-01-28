%:- use_module([library(semweb/rdf_prefixes)]).
:- use_module([library(writef)]).

:- object(test_block,
          specializes(code_block)).
:- end_object.


:- object(tinst, instantiates(code_block)).
:- end_object.


:- object(tests, extends(lgtunit)).
:- info([
               version is 0.1,
               author is 'Evgeny Cherkashin',
               date is 2017/01/22,
               comment is 'Unit test for a Python code generator.'
           ]).

:- discontiguous
       succeeds/1,
       fails/1.
:- private([attr/1]).
:- dynamic([attr/1]).

succeeds(setup_add_option_1):-
    setup::set(option1=value1).

succeeds(setup_add_option_2):-
    setup::set(option2,value2).

succeeds(setup_add_option_3):-
    setup::set(option3-value3).

succeeds(setup_add_option_4):-
    setup::set(option4/value4).

succeeds(setup_get_option_1):-
    setup::option(option4,value4).

succeeds(setup_get_options):-
    setup::options(List),
    List=[option1=value1,option2=value2,
          option3=value3,option4=value4].

succeeds(setup_get_nonexistent_option_as_default):-
    setup::option(option_none, Value, default),
    Value=default.

succeeds(setup_clear):-
    setup::clear.

succeeds(setup_get_options_empty):-
    setup::options([]).

succeeds(setup_add_option_tab_size):-
    setup::set(tab_size, 4).

succeeds(setup_setup_globally):-
    root::setup(setup).

succeeds(setup_check_indent_1):-
    root::indent("").

succeeds(setup_check_indent_2):-
    root::indent,
    root::indent(S).

succeeds(setup_check_indent_3):-
    root::indent,
    root::indent("\t\t").

succeeds(setup_check_indent_4):-
    root::unindent,
    root::indent("\t").

succeeds(setup_check_indent_5):-
    root::unindent,
    root::indent("").

succeeds(setup_change_way_of_indent):-
    setup::set(use_tabs, false).

succeeds(setup_check_way_of_indent):-
    setup::option(use_tabs, false).

succeeds(setup_check_indent_6):-
    root::indent("").

succeeds(setup_check_indent_7):-
    root::indent,
    root::indent(S).

succeeds(setup_check_indent_8):-
    root::indent,
    root::indent("        "),
    root::unindent,
    root::unindent,
    root::indent("").

succeeds(block_add_1) :-
    tinst::append(1).

succeeds(block_prepend) :-
    tinst::prepend(0).

succeeds(block_add_2) :-
    tinst::append(2).

succeeds(block_list):-
    findall(X, tinst::item(X), L),
    % writef::writef(':::: %w',[L]),
    L == [0,1,2].

succeeds(test_clear) :-
    tinst::clear.

succeeds(block_list_2):-
    findall(X, tinst::item(X), []).

succeeds(test_render):-
    tinst::render(_).

succeeds(setup_bad_option_set):-
    setup::set(tab_size, -100).

%fails(test_render_with_bad_option):-
%    tinst::render(setup,_).

succeeds(setup_restore_option_tab_size):-
    setup::set(tab_size, 4).

succeeds(render_again):-
    tinst::render(_).

succeeds(create_param_1):-
    create_object(aparam, [instantiates(param)],[],[]).

succeeds(setup_param_1):-
    aparam::name(name),
    aparam::type('String'),
    aparam::default("Default").

succeeds(check_param_1):-
    aparam::items(L),
    % write(L),
    L=[name(name),type('String'),default("Default")].

succeeds(render_param_1):-
    aparam::render(String),
    % writef::writef("!!!!Result: %w", [String]),!,
    String="name:String=\"Default\"".

succeeds(create_param_2):-
    create_object(bparam,[instantiates(param)],[],[]),
    bparam::name(id),
    bparam::type(int),
    bparam::items(BItems),
    BItems=[name(id),type(int)].


succeeds(create_param_3):-
    create_object(cparam,[instantiates(param)],[],[]),
    cparam::name(name),
    cparam::type('String'),
    cparam::default(""),
    cparam::items(CItems),
    CItems=[name(name),type('String'),default("")].


succeeds(create_params_1):-
    create_object(params1,[instantiates(params)],[],[]),
    params1::append(cparam),
    params1::append(bparam).

succeeds(render_params_1):-
    params1::renderaslist(", ", Result),
    % writef::writef('Render result: %p\n',[Result]),
    Result="name:String=\"\", id:int".

succeeds(render_params_again_1):-
    params1::renderaslist(", ", Result),
    Result="name:String=\"\", id:int".

succeeds(create_method_body):-
    create_object(methodbody,[instantiates(body)],[],[]),
    methodbody::append(pass).

succeeds(render_method_body):-
    methodbody::render(["pass"]).

succeeds(render_method_1):-
    create_object(methodadd, [instantiates(method)],[],[]),
    methodadd::name(add),
    methodadd::type(bool),
    methodadd::params(params1),
    methodadd::body(methodbody),
    methodadd::render(Result),
    Result = ["def add(name:String=\"\", id:int) -> bool:","    pass"].

succeeds(render_method_body_indent_check):-
    methodbody::render(["pass"]).

succeeds(create_class):-
    create_object(aclass, [instantiates(class)],[],[]).

succeeds(render_class_1):-
    aclass::render(L),
    L="a class".


:- end_object.
