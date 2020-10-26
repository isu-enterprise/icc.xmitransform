%%%%%%%%%%%%%%%%%%%% Global configuration object prototype %%%%%%%%%%%%%%%%%%%%5

:- object(config).
    :- public([
        option/2, % Set an option to value
        option/1, % Set in form of <option>=<value>
        clear/0,
        remove/1
    ]).
    :- protected([
        option_/1  % An option in form of option_(<option>=<value>).
    ]).
    :- dynamic([
        option_/1
    ]).

    option(Option, Value):-
        ::remove(Option),
        ::assertz(option_(Option=Value)).

	option(Option=Value):-
		option(Option,Value).

	option(Option/Value):-
		option(Option,Value).

	option(Option-Value):-
		option(Option,Value).

    remove(Option):-
        ::retractall(option_(Option=_)).

    clear:-
        ::retractall(option_(_)).

	:- public([
	   current_option/2,
	   current_option/1,
	   current_option/3
	]).

    current_option(Option, Value):-
        ::option_(Option=Value).

    current_option(Option, Value, Default):-
        var(Value),
        (
            current_option(Option, Value) ->
            true;
            Value = Default
        ),!.

    current_option(Option, Value, Default):-
        nonvar(Value),
        (
            current_option(Option, _Value) ->
            (
                Value = _Value ->
                true;
                fail
            );
            Value = Default
        ),!.


    current_option(Option):-
        ::option_(Option).

    :- protected(setup/0).
    setup.

    :- initialization(setup).

:- end_object.

%%%% Category Current Option %%%%
:- category(current_option).

    :- public(current_option/1).
    current_option(Option=Value):-
        ::current_config(Setup),
        Setup::current_option(Option=Value).

    :- public(current_option/2).
    current_option(Option,Value):-
        ::current_option(Option=Value).

:- end_category.
