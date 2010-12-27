-module(clients).
-compile(export_all).

-import(dict).
-import(mnesia).
-include("chatroom.erl").

%The information associated to a single client
-record(client_info, {name, id, chat}).

%The state associated with all clients
-record(client_state, {clients, name_to_pid}).

lookup_client_by_pid(ClientState, Pid) ->
	dict:fetch(Pid, ClientState#client_state.clients).

lookup_client_by_name(ClientState, Name) ->
	Pid = dict:fetch(Name, ClientState#client_state.name_to_pid),
	lookup_client_by_pid(ClientState, Pid).

remove_client(ClientState, Pid) ->
	Info = dict:fetch(Pid, clients_pid),
	ClientState#client_state{ 
		clients = dict:erase(Pid, ClientState#client_state.clients),
		name_to_pid = dict:erase(Info#client_info.name)}.

register_client(ClientState, Name) ->
	ClientState.

%This is the loop associated to a particular client
client_loop(ClientInfo, PendingEvents) ->
	receive
		case {heartbeat, DispatchPID} ->
			DispatchPID ! {event_list, PendingEvents},
			client_loop(ClientInfo, []);

		
		after 60000 ->
			timeout
	end.
	
	


start_login_server() ->
	ok.
	

