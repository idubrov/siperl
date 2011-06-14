-module(user_default). 

-export([help/0, lm/0, mm/0, ctl/1, ctf/1, cc/1]).

help() -> 
    shell_default:help(), 
    io:format("** user extended commands **~n"), 
    io:format("lm()  -- load all changed modules~n"), 
    io:format("mm()  -- list modified modules~n"), 
	io:format("ct(M) -- run EUnit on given module and analyse coverage~n"),
    true. 

lm() ->
    [c:l(M) || M <- mm()].

mm() ->
    modified_modules().

ct(Module, Level) ->
	cc(Module),
	eunit:test(Module),
	{ok, Items} = cover:analyze(Module, coverage, Level),
	[{Item, {Cov, NotCov}} || {Item, {Cov, NotCov}} <- Items, NotCov > 0].

ctl(Module) ->
	ct(Module, line).

ctf(Module) ->
	ct(Module, function).

modified_modules() ->
    [M || {M, _} <- code:all_loaded(), module_modified(M) == true].

module_modified(Module) ->
    case code:is_loaded(Module) of
		{file, preloaded} ->
			false;
		{file, cover_compiled} ->
			true;
		{file, Path} ->
			CompileOpts = proplists:get_value(compile, Module:module_info()),
			CompileTime = proplists:get_value(time, CompileOpts),
			Src = proplists:get_value(source, CompileOpts),
			module_modified(Path, CompileTime, Src);		
		_ ->
			false
	end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
		false ->
			false;
		ModPath ->
			case beam_lib:chunks(ModPath, ["CInf"]) of
				{ok, {_, [{_, CB}]}} ->
					CompileOpts = binary_to_term(CB),
					CompileTime = proplists:get_value(time, CompileOpts),
					Src = proplists:get_value(source, CompileOpts),
					not (CompileTime == PrevCompileTime) and (Src == PrevSrc);
				_ ->
					false
			end
	end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
		{ok, _} ->
			Path;
		_ ->
			%% may be the path was changed?
			case code:where_is_file(filename:basename(Path)) of
				non_existing ->
					false;
				NewPath ->
					NewPath
			end
	end.

cc(Module) ->
	case code:which(Module) of
		cover_compiled ->
			code:delete(Module);
		
		_ ->
			true
	end,
	Path = code:which(Module),
	{ok, {Module, CompileInfo}} = beam_lib:chunks(Path, [compile_info]),
	CompileOpts = proplists:get_value(compile_info, CompileInfo),
	Src = proplists:get_value(source, CompileOpts),
	Options = proplists:get_value(options, CompileOpts),
	cover:compile_module(Src, Options),
	code:purge(Module).
