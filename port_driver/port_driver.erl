-module(port_driver).
-export([start/1, stop/0, init/1]).
-export([foo/1, bar/1]).
start(SharedLib) ->
    case erl_ddll:load(".", SharedLib) of
	ok -> ok;
	{error, already_loaded} -> ok;
	_ -> exit({error, could_not_load_driver})
    end,
    spawn(?MODULE, init, [SharedLib]).
init(SharedLib) ->
    register(complex, self()),
    Port = open_port({spawn, SharedLib}, [binary]),
    loop(Port).
stop() ->
    complex ! stop.
foo(X) ->
    call_port({foo, X}).
bar(Y) ->
    call_port({bar, Y}).
call_port(Msg) ->
    complex ! {call, self(), Msg},
    error_logger:info_msg("waiting for result~n"),
    receive
	{complex, Result} ->
	    Result;
        X -> error_logger:info_msg("got ~w ~n",[X])
    end.
loop(Port) ->
    receive
	{call, Caller, Msg} ->
            port_command(Port,<<"a">>),
	    receive
		Data ->
		    Caller ! {complex, Data}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    io:format("~p ~n", [Reason]),
	    exit(port_terminated)
    end.
