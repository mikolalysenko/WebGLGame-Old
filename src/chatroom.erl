-module(chatroom).

-compile(export_all).

-import(erlang).
-import(lists).
-import(dict).
-import(timer).

%The state of a chatroom
-record(room_state, {room_name, log, listeners}).

%Initializes the room state
create_room_state(RoomName) ->
	#room_state{ room_name=RoomName, log=[], listeners=dict:new() }. 

%adds an event to the room state
add_event(State, Sender, Text) ->
	
	RoomName = State#room_state.room_name,
	Listeners = State#room_state.listeners,
	
	%Log the event to the terminal
	io:format("(~s,~s): ~s\n", [RoomName, Sender, Text] ),
	
	%Broadcast event to each listener
	lists:foreach(
		fun (Listener) ->
			Listener ! 
				{chat, 
					State#room_state.room_name,
					Sender,
					Text}
		end, dict:fetch_keys(Listeners)),
		
	State.
	

%Adds a listener to the room state
add_listener(State, Pid) ->

	Listeners = State#room_state.listeners,
	
	case dict:is_key(Pid, Listeners) of
		true ->	
			State;
		false ->
			Ref = erlang:monitor(process, Pid),
			State#room_state{ listeners = dict:store( Pid, Ref, Listeners ) }
	end.

%Removes a listener
remove_listener(State, Pid) ->

	Listeners = State#room_state.listeners,
	
	case dict:is_key(Pid, Listeners) of
		true ->
			Ref = dict:fetch(Pid, Listeners),
			erlang:demonitor(Ref),
			State#room_state{ listeners = dict:erase(Pid, Listeners) };
		false ->
			State
	end.

%The main loop for a chat server
chat_loop(State) ->
	receive
		{message, Sender, Text} ->
			io:format("Got message\n"),
			chat_loop(add_event(State, Sender, Text));
			
		{register_listener, Pid} ->
			io:format("Adding listener, ~p\n", [Pid]),
			chat_loop(add_listener(State, Pid));
	
		{unregister_listener, Pid} ->
			io:format("Removing listener, ~p\n", [Pid]),
			chat_loop(remove_listener(State, Pid));
		
		{'DOWN', _, process, Pid, _} ->
			io:format("Listener died, ~p\n", [Pid]),
			chat_loop(remove_listener(State, Pid));
			
		kill ->
			ok
	end.


%Starts a chat server process
create_room(RoomName) ->
	State = create_room_state(RoomName),
	spawn(?MODULE, chat_loop, [State] ).

%Exported function for chatting with a server
say(RoomPid, Sender, Text) ->
	RoomPid ! {message, Sender, Text}.
	
%Exported function for registering a listener
join_room(RoomPid, Pid) ->
	RoomPid ! {register_listener, Pid}.

%Exported function for removing a listener
leave_room(RoomPid, Pid) ->
	RoomPid ! {unregister_listener, Pid}.

%Kills the chat room	
kill_room(RoomPid) ->
	RoomPid ! kill.


