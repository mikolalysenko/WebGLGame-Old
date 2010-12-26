%This module is the main game loop.
-module(simulator).
-compile(export_all).

-include("clients.erl").
-include("chatroom.erl").

-record{game_state, {client_data}).

%Spawns and registers the simulator process
start() ->
	Pid = spawn(?MODULE, start_simulator, []),
	register(sim_pid, Pid),	
	Pid.

%The timer process, just sends a tick event every 100ms
ticker(Pid) ->
	receive
	after 500 ->
		Pid ! tick
	end,
	ticker(Pid).

%This creates the initial gamestate
create_initial_gamestate() ->
	#game_state{clients=dict:new()}.

%Starts the simulator
start_simulator() ->
	%Register the exit flag
	process_flag(trap_exit, true),

	%Create the initial gamestate
	State = create_initial_gamestate(),
	
	%Start the chat server process
	GlobalChatPID = chatroom:start_chat_server("Global"),
	register(global_chat_pid, GlobalChatPID),
	link(GlobalChatPID),
	
	%Spawn the ticker and start the simulator loop
	spawn(?MODULE, ticker, [self()]),
	sim_loop(State).


do_login(State, DispatchPID, Name) ->
	State.
	
do_logout(State, DispatchPID, ClientID) ->
	State.
	
%Dispatch a chat message.  Currently, there is only private messaging and global chat
do_chat(State, DispatchPID, ClientID, Target, Message) ->
	Client = clients:lookup_client_by_pid(State#game_state.client_state, ClientID),
	Name = Client#client_info.name,
	case Target of
		global_chat ->
			global_chat_pid ! {message, Name, Message};
		
		_ ->
			case clients:lookup_client_by_name(State#game_state.client_state, Target) of
				not_found ->
					Client#client_info.chat ! {message, "System", "Invalid target for chat command"};
				TargetInfo ->
					TargetInfo#client_info.chat !  {message, Name, Message}
			end
	end,
	State.
	
%The simulator loop, recieves events from clients and ticks and updates the game state.
sim_loop(State) ->
	receive 
		tick ->
			io:format("Got tick\n"),
			sim_loop(State);
			
		{login, DispatchPID, Name} ->
			io:format("Got login: ", Name, "\n"),
			sim_loop(do_login(State, DispatchPID, Name));
			
		{logout, DispatchPID, ClientID} ->
			io:format("Got log out event"),
			sim_loop(do_logout(State, DispatchPID, ClientID));
			
		{heartbeat, DispatchPID, ClientID} ->
			ClientID ! {heartbeat, DispatchPID},
			sim_loop(State);
			
		{chat, DispatchPID, ClientID, Target, Message} ->
			io:format("Got chat message: ", Message),
			sim_loop(do_chat(State, DispatchPID, ClientID, Target, Message));
			
		{'EXIT', FromPID, } ->
			{ok}
	end.

