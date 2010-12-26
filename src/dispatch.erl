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
			sim_pid ! {heartbeat, self(), ClientID};
		
		%Chat command
		% Path = c
		% m = message data
		% t = optional target data
		"/c" ->
			case queryvar(A, "m") of
				{ok, Message} ->
					case queryvar(A, "t") of
						{ok, Target} ->
							sim_pid ! {chat, self(), ClientID, Target, Message};
						undefined ->
							sim_pid ! {chat, self(), ClientID, global_chat, Message}
					end;
				undefined ->
					[].
			end;
			
		%Log out command
		"/l" ->
			sim_pid ! {logout, self(), ClientID}
	end.

%Translates and pushes updates out to the client	
update_client_state(Data) ->
	[html, ""].

%Default dispatch code
out(A) ->
	H = A#arg.headers,
	C = H#headers.cookie,
	
	case check_login_cookie(C) of
		false ->
			handle_login(A);
		{true, ClientID} ->
			process_action(ClientID, A),
			receive
				{event_list, Data} -> update_client_state(Data)
			end
	end.

