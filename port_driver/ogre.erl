-module(ogre).
-export([load/0,run_process/1]).
-export([init_ogre/0, destroy_ogre/0,render_frame/0,capture_input/0,key_down/1]).
load() ->
    case erl_ddll:load(".", ogre_driver) of
	ok -> ok;
	{error, already_loaded} -> ok;
	_ -> exit({error, could_not_load_driver})
    end,
    spawn(?MODULE, run_process, [ogre_driver]).
run_process(SharedLib) ->
    register(complex, self()),
    Port = open_port({spawn, SharedLib}, [binary]),
    loop(Port).

%stop() ->
%    complex ! stop.

init_ogre() ->
    call_port(<<1/little>>).
destroy_ogre() ->
    call_port(<<2/little>>).
render_frame() ->
    call_port(<<3/little>>).
capture_input() ->
    call_port(<<4/little>>).
key_down(Key) ->
    call_port(<<5/little,Key/little>>).

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
            port_command(Port,Msg),
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
