%This module is the main game loop.
-module(simulator).
-export([start/0]).

start() ->
	%Spawn the simulator
	simulator_pid = spawn(?MODULE, loop, []),
	global:register_name("simulator", simulator_pid),
	
	%Spawn the ticker
	ticker_pid = spawn(?MODULE, ticker, [simulator_pid]).

%The timer process, just sends a tick event every 100ms
ticker(simulator_pid) ->
	receive
	after 100 ->
		simulator_pid ! tick
	end,
	simulator:ticker(simulator_pid).


%The simulator loop, recieves events from clients and ticks and updates the game state.
loop() ->
	receive 
		tick ->
			io:format("got tick")
	end,
	simulator:loop().

