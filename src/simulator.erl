%This module is the main game loop.
-module(simulator).
-compile(export_all).


start() ->
	%Spawn the simulator
	Pid = spawn(?MODULE, loop, []),
	register(sim_pid, Pid),
	
	%Spawn the ticker
	spawn(?MODULE, ticker, [Pid]),
	Pid.

%The timer process, just sends a tick event every 100ms
ticker(Pid) ->
	receive
	after 500 ->
		Pid ! tick
	end,
	ticker(Pid).


%The simulator loop, recieves events from clients and ticks and updates the game state.
loop() ->
	receive 
		tick ->
			io:format("Got tick\n");
		{move, ClientId} ->
			io:format("Got move event\n")
	end,
	loop().

