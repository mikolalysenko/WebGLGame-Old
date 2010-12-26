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


%A test process for the chat server
dummy_listener() ->
	receive
		{chat, Room, Sender, Message} ->
			io:format("Dummy listener ~p, got: (~s,~s) -- ~s\n", [self(), Room, Sender, Message]),
			dummy_listener();
			
		kill -> 
			io:format("Dummy listener ~p, stopping\n", [self()]),
			ok
	end.

start_dummy() ->
	spawn(?MODULE, dummy_listener, []).

test() ->
	C = create_room("test"),
	io:format("Created room ~p\n", [C]),
	D = start_dummy(),
	io:format("Created dummy ~p\n", [D]),
	join_room(C, D),
	say(C, "foo", "bar"),
	leave_room(C, D),
	say(C, "foo", "b2"),
	join_room(C, D),
	join_room(C, D),
	say(C, "test", "again"),
	E = start_dummy(),
	io:format("Created dummy ~p\n", [E]),
	join_room(C, E),
	say(C, "b", "A"),
	leave_room(C, D),
	say(C, "1", "2"),
	Q = start_dummy(),
	join_room(C, Q),
	join_room(C, D),
	say(C, "p", "q"),
	timer:sleep(30),
	D ! kill,
	say(C, "x", "y"),
	timer:sleep(30),
	Q ! kill,
	E ! kill,
	timer:sleep(30),
	kill_room(C).

