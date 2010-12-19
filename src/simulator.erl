%This module is the main game loop.
-module(simulator).
-compile(export_all).

start() ->
	%Spawn the simulator
	Pid = spawn(?MODULE, loop, []),
	
	%Spawn the ticker
	spawn(?MODULE, ticker, [Pid]),
	Pid.

%The timer process, just sends a tick event every 100ms
ticker(Pid) ->
	receive
	after 100 ->
		Pid ! tick
	end,
	ticker(Pid).


%The simulator loop, recieves events from clients and ticks and updates the game state.
loop() ->
	receive 
		tick ->
			io:format("got tick\n")
	end,
	loop().

