-module(dispatch).

-include_lib("/usr/local/lib/yaws/include/yaws_api.hrl").
-compile(export_all).

out(A) ->
	io:format("got message."),
	{html, "foo"}.

