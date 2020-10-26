:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tutor(loader)),
    logtalk_load(debugger(loader)),  % debugging
	logtalk_load([config,xmi,queries,model,gen,psmmothur,test], [dynamic_declarations(allow), debug(on), source_data(on)]),
	logtalk_load(tests, [hook(lgtunit),dynamic_declarations(allow), debug(on), source_data(on)]),
	tests::run,
%    mothurrdftests::run,
    true
)).

%:- halt.
