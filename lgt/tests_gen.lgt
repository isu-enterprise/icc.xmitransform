%:- use_module([library(semweb/rdf_prefixes)]).
:- use_module([library(writef)]).

:- object(test_block,
          specializes(code_block)).
:- end_object.


:- object(tinst, instantiates(code_block)).
:- end_object.


:- object(tests, extends(lgtunit)).
:- info([
               version is 0:2:0,
               author is 'Evgeny Cherkashin',
               date is 2017-01-22,
               comment is 'Unit test for a Python code generator.'
           ]).

:- discontiguous
       succeeds/1,
       fails/1.
:- private([attr/1]).
:- dynamic([attr/1]).

succeeds(config_add_option_1):-
    config::option(option1=value1).

succeeds(config_add_option_2):-
    config::option(option2,value2).

succeeds(config_add_option_3):-
    config::option(option3-value3).

succeeds(config_add_option_4):-
    config::option(option4/value4).

succeeds(config_get_option_1):-
    config::current_option(option4,value4).

succeeds(config_get_nonexistent_option_as_default):-
    config::current_option(option_none, Value, default),
    Value=default.

succeeds(config_clear):-
    config::clear.

succeeds(config_add_option_tab_size):-
    config::option(tab_size, 4).

succeeds(config_config_globally):-
    root::config(config).

succeeds(config_check_indent_1):-
    root::indent("").

succeeds(config_check_indent_2):-
    root::indent,
    root::indent("\t").

succeeds(config_check_indent_3):-
    root::indent,
    root::indent("\t\t").

succeeds(config_check_indent_4):-
    root::unindent,
    root::indent("\t").

succeeds(config_check_indent_5):-
    root::unindent,
    root::indent("").

succeeds(config_change_way_of_indent):-
    config::option(use_tabs, false).

succeeds(config_check_way_of_indent):-
    config::option(use_tabs, false).

succeeds(config_check_indent_6):-
    root::indent("").

succeeds(config_check_indent_7):-
    root::indent,
    root::indent("    ").

succeeds(config_check_indent_8):-
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

fails(test_render):-
    tinst::render(_).

succeeds(config_bad_option_set):-
    config::option(tab_size, -100).

succeeds(config_restore_option_tab_size):-
    config::option(tab_size, 4).

fails(render_again):-
    tinst::render(_).

succeeds(create_param_1):-
    create_object(aparam, [instantiates(param)],[],[]).

succeeds(config_param_1):-
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
    params1::render(Result),
    % writef::writef('Render result: %p\n',[Result]),
    Result="name:String=\"\", id:int".

succeeds(render_params_again_1):-
    params1::render(Result),
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
    %format("-----------> ~q", [Result]),!,
    Result = ["def add(self, name:String=\"\", id:int) -> bool:","    pass"].

succeeds(render_method_body_indent_check):-
    methodbody::render(["pass"]).

succeeds(create_class_list):-
    create_object(importclasses, [instantiates(classlist)],[],[]),
    importclasses::append('object1'),
    importclasses::append('object2').

succeeds(render_class_list):-
    importclasses::render(String),
    String="object1, object2".

succeeds(create_method_list):-
    create_object(amethodlist, [instantiates(methodlist)],[],[]),
    amethodlist::append(methodadd).

succeeds(create_class):-
    create_object(aclass, [instantiates(class)],[],[]),
    aclass::name('AClass'),
    aclass::classlist(importclasses),
    aclass::methods(amethodlist).

succeeds(render_class_1):-
    aclass::render(_).
    %writef::writef('CLASS:\n%w\n',[L]).

succeeds(dotted_name_create):-
    create_object(ospath, [instantiates(dottedname)],[],[]),
    ospath::append(literal(os)),
    ospath::append(literal(path)),
    ospath::render("os.path").

succeeds(import_create):-
    create_object(importospath, [instantiates(import)],[],[]),
    importospath::import(ospath),
    importospath::importas(sqlite3, sql).

succeeds(import_render_1):-
    importospath::render(["import os.path","import sqlite3 as sql"]).
% writef::writef('Import:\n%w\n',[String]).

succeeds(import_add_1):-
    importospath::from(ospath, getcwd),
    importospath::from(sqlite3, conn, connect).

succeeds(import_render_2):-
    importospath::render(["import os.path",
                          "import sqlite3 as sql",
                          "from os.path import getcwd",
                          "from sqlite3 import conn as connect"]).
%    writef::writef('Import:\n%w\n',[String]).



:- end_object.
