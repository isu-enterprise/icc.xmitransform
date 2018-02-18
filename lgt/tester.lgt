:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(lgtunit(loader)),
	logtalk_load([xmi,queries,model,test], [dynamic_declarations(allow), debug(on), source_data(on)]),
	logtalk_load(tests, [hook(lgtunit),dynamic_declarations(allow), debug(on), source_data(on)]),
	tests::run
)).

%:- halt.
