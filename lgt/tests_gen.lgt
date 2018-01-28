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

succeeds(setup_get_options_nonempty):-
    setup::options(List),
    List=[option1=value1,option2=value2,
          option3=value3,option4=value4].

succeeds(setup_clear):-
    setup::clear.

succeeds(setup_get_options_empty):-
    setup::options([]).

succeeds(setup_add_option_tab_size):-
    setup::set(tab_size, 4).

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
    tinst::render(setup,_).

succeeds(setup_bad_option_set):-
    setup::set(tab_size, -100).

%fails(test_render_with_bad_option):-
%    tinst::render(setup,_).

succeeds(setup_restore_option_tab_size):-
    setup::set(tab_size, 4).

succeeds(render_again):-
    tinst::render(setup,_).

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
    aparam::render(setup, String),
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
    params1::renderaslist(setup, ", ", Result),
    % writef::writef('Render result: %p\n',[Result]),
    Result="name:String=\"\", id:int".

succeeds(render_params_again_1):-
    params1::renderaslist(setup, ", ", Result),
    % writef::writef('Render result: %p\n',[Result]),
    Result="name:String=\"\", id:int".


:- end_object.
