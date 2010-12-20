-module(dispatch).
-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").
-include("simulator.erl").


check_login_cookie(C) ->
	{true, "0"}.

handle_login(A) ->
	[html, "Not logged in."].
	
process_action(ClientID, A) ->
	case A#arg.appmod of
		%Heartbeat / do nothing action.  Resets timeout counter
		"/t" -> 
			[];
		
		%Chat command
		% Path = c
		% m = message data
		% t = optional target data
		"/c" ->
			case queryvar(A, "m") of
				{ok, Message} ->
					case queryvar(A, "t") of
						{ok, Target} ->
							sim_pid ! {priv_msg, ClientID, Target, Message};
						undefined ->
							sim_pid ! {chat, ClientID, Message}
					end;
				undefined ->
					[].
			end
	end.
	
update_client_state(ClientID) ->
	[html, ""].

out(A) ->
	H = A#arg.headers,
	C = H#headers.cookie,
	
	case check_login_cookie(C) of
		false ->
			handle_login(A);
		{true, ClientID} ->
			process_action(ClientID, A),
			update_client_state(ClientID)
	end.

