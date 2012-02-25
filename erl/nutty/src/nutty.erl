-module(nutty).
-export([main/1]).


main(Args) ->
    {ok, Port} = get_port(Args),
    {ok, Conn} = connect(Port),
    loop(Conn).


loop(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, Packet} ->
            handle(Packet);
        {error, closed} ->
            halt(0);
        {error, _} ->
            halt(2)
    end,
    loop(Conn).


handle(Packet) ->
    io:format("Received packet.~n", []).


connect(Port) ->
    gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}, {packet, 4}]).


get_port([]) ->
    halt(1);
get_port([_]) ->
    halt(1);
get_port(["--port", Port]) ->
    {ok, list_to_integer(Port)};
get_port([_ | Rest]) ->
    get_port(Rest).
