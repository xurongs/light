-module(config).
-export([read_from_file/2]).

read_from_file(File, Path) ->
	case file:path_open(Path, File, [read]) of
		{ok, Stream, FullName} ->
			Return = 
				case systools_lib:read_term_from_stream(Stream, File) of
					{ok, Term} ->
						{ok, Term, FullName};
					Other ->
						Other
				end,
			file:close(Stream),
			Return;
		_Other ->
			{error, {not_found, File}}
	end.
