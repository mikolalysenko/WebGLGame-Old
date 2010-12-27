%This module is the main game loop.
-module(simulator).
-compile(export_all).

-import(application).
-import(mnesia).

%The game state structure
-record(game_state, { foo }).

%Spawns and registers the simulator process
start() ->

	%Initialize the database
	mnesia:start(),
	ok = mnesia:wait_for_tables([rec], 2000),

	%Spawn simulator process
	spawn(?MODULE, start_simulator, []).

%The timer process, just sends a tick event every 100ms
ticker(Pid) ->
	receive
	after 500 ->
		Pid ! tick
	end,
	ticker(Pid).

%This creates the initial gamestate
create_initial_gamestate() -> #game_state{ }.

%Starts the simulator
start_simulator() ->

	%Register simulator pid
	register(sim_pid, self()),	

	%Register the exit flag
	process_flag(trap_exit, true),

	%Create the initial gamestate
	State = create_initial_gamestate(),
	
	%Start the chat server process
	GlobalChatPID = chatroom:start_chat_server("Global"),
	register(global_chat_pid, GlobalChatPID),
	link(GlobalChatPID),
	
	%Start ticking
	spawn_link(?MODULE, ticker, [self()]),
	
	%Jump into the simulation loop
	sim_loop(State).
	
	
%The simulator loop, recieves events from clients and ticks and updates the game state.
sim_loop(State) ->
	receive 
		tick ->
			io:format("Got tick\n"),
			sim_loop(State);
					
		{'EXIT', _, _} ->
			{ok}
	end.

