-module(test_driver).

-export([load/1, start_link/2, stop/1, unload/1]).
-export([foo/3, bar/3]).

load(Driver) ->
    Dir = case code:priv_dir(tracerl) of
              {error, bad_name} -> "priv";
              Path -> Path
          end,
    case erl_ddll:load_driver(Dir, Driver) of
	ok -> ok;
	{error, already_loaded} -> ok;
	{error, Error} ->
            error_logger:error_msg(erl_ddll:format_error(Error) ++ "\n"),
            {error, Error}
    end.

unload(Driver) ->
    case erl_ddll:unload_driver(Driver) of
        ok -> ok;
        {error, Error} ->
            error_logger:error_msg(erl_ddll:format_error(Error) ++ "\n"),
            {error, Error}
    end.

start_link(Driver, Name) ->
    spawn_link(fun() -> init(Driver, Name) end).

init(Driver, Name) ->
    register(Name, self()),
    Port = open_port({spawn, Driver}, [binary]),
    loop(Name, Port).

stop(Name) ->
    Name ! stop.

foo(Name, Method, Arg) ->
    call_port(Name, {Method, foo, Arg}).
bar(Name, Method, Arg) ->
    call_port(Name, {Method, bar, Arg}).

call_port(Name, Msg) ->
    Name ! {call, self(), Msg},
    receive
	{Name, Result} ->
	    Result
    end.

loop(Name, Port) ->
    receive
        {call, Caller, {control, Function, Arg}} ->
            [Res] = port_control(Port, encode_function(Function), [Arg]),
            Caller ! {Name, Res},
            loop(Name, Port);
        {call, Caller, {call, Function, Arg}} ->
            Res = erlang:port_call(Port, encode_function(Function), Arg),
            Caller ! {Name, Res},
            loop(Name, Port);
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {Name, decode(Data)}
	    end,
	    loop(Name, Port);
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

encode({Method, Function, Arg}) ->
    [case Method of
         sync -> 1;
         async -> 2
     end,
     encode_function(Function),
     Arg].

encode_function(foo) -> 1;
encode_function(bar) -> 2.

decode(<<Int>>) -> Int.
