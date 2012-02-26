-module(nutty).
-export([main/1]).


main(Args) ->
    %application:start(sasl),
    {ok, Port} = get_port(Args),
    {ok, Conn} = connect(Port),
    try loop(Conn)
    catch _:_ ->
        halt(3)
    end.


loop(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, Packet} ->
            handle(Conn, binary_to_term(Packet));
        {error, closed} ->
            halt(0);
        {error, _} ->
            halt(2)
    end,
    loop(Conn).


handle(Conn, {call, Timeout,  Mod, Fun, Args}) ->
    run(Conn, Timeout, fun() -> erlang:apply(Mod, Fun, Args) end);
handle(Conn, {compile, Mod, Source}) ->
    case code:is_loaded(Mod) of
        {file, _} ->
            send(Conn, ok);
        false ->
            case build(binary_to_list(Source)) of
                ok -> send(Conn, ok);
                Else -> send(Conn, Else)
            end
    end;
handle(Conn, {run, Timeout, Mod, Arg}) ->
    run(Conn, Timeout, fun() -> Mod:main(Arg) end);
handle(Conn, Command) ->
    send(Conn, {unknown_command, Command}).


send(Conn, Term) ->
    gen_tcp:send(Conn, term_to_binary(Term, [{minor_version, 1}])).


connect(Port) ->
    Opts = [binary, {active, false}, {packet, 4}],
    gen_tcp:connect("127.0.0.1", Port, Opts).


get_port([]) ->
    halt(1);
get_port([_]) ->
    halt(1);
get_port(["--port", Port]) ->
    {ok, list_to_integer(Port)};
get_port([_ | Rest]) ->
    get_port(Rest).


run(Conn, Timeout, Fun) ->
    {Pid, Ref} = spawn_monitor(fun() -> exit(Fun()) end),
    receive {'DOWN', Ref, _, Pid, Resp} ->
        send(Conn, {resp, Resp})
    after Timeout ->
        exit(Pid, kill),
        erlang:demonitor(Ref, [flush]),
        send(Conn, timeout)
    end.


build(Source) ->
    try
        {ok, Mod, Bin} = compile(parse(scan(Source))),
        code:purge(Mod),
        code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
        ok
    catch
        throw:Error -> Error;
        Type:Error -> {Type, Error}
    end.


scan(Source) ->
    case (catch erl_scan:string(Source)) of
        {ok, Tokens, _} ->
            Tokens;
        Else ->
            throw({error, Else})
    end.


parse(Tokens) ->
    case (catch erl_parse:parse_exprs(Tokens)) of
        {ok, Forms} ->
            Forms;
        Else ->
            throw({error, Else})
    end.


compile(Forms) ->
    case compile:forms(Forms) of
        {ok, ModName, Beam} ->
            {ok, ModName, Beam};
        Else ->
            throw({error, Else})
    end.

