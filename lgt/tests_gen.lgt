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
       fails/1
       .

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

succeeds(test_render_again):-
    tinst::render(setup,_).


:- end_object.
