-module(dispatch).
-export([out/1]).

-import(simulator).
-include_lib("yaws/include/yaws_api.hrl").


%Default dispatch code
out(A) ->
	[html, ""].

