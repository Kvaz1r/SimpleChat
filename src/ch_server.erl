%%%-------------------------------------------------------------------
%%% Created : 02. Март 2016 19:46
%%%-------------------------------------------------------------------
-module(ch_server).
-export([start/1]).

start(Port) ->
  register(?MODULE, spawn(fun() -> server(sets:new()) end)),
  listen(Port).

listen(Port) ->
  {ok, Socket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
  spawn_link(fun() -> accept(Socket) end),
  Socket.

accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  Pid = spawn(fun() -> loop(Socket) end),
  gen_tcp:controlling_process(Socket, Pid),
  accept(LSocket).

loop(Sock) ->
  inet:setopts(Sock, [{active, once}]),
  receive
    {tcp, Socket, Data} ->
      NewConnection = erlang:term_to_binary({add, new}),
      case Data of
        NewConnection -> ?MODULE ! {add, Socket};
        _ -> ?MODULE ! {send, Data}
      end,
      loop(Socket);
    {tcp_closed, Socket} ->
      ?MODULE ! {remove, Socket}
  end.

server(Set) ->
  receive
    {add, Socket} ->
      NewSet = sets:add_element(Socket, Set),
      server(NewSet);
    {remove, Socket} ->
      NewSet = sets:del_element(Socket, Set),
      server(NewSet);
    {send, Message} ->
      [gen_tcp:send(Socket, Message) || Socket <- sets:to_list(Set)],
      server(Set)
  end.