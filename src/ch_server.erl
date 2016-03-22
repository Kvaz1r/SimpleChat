%%%-------------------------------------------------------------------
%%% Created : 02. Март 2016 19:46
%%%-------------------------------------------------------------------
-module(ch_server).
-export([start/1, stop/0]).

start(Port) ->
  register(?MODULE, spawn(fun() ->
    {ok, File} = file:open("newinfo", [append]),
    server(sets:new(), File) end)),
  listen(Port).

stop() -> stop([]).

stop(Reason) ->
  case whereis(?MODULE) of
    undefined -> ok;
    _ ->
      case Reason =:= [] of
        true -> ?MODULE ! {add_info, "Stop server ~n"};
        false ->
          ?MODULE ! {add_info, lists:flatten(
            io_lib:format("Stop server by error on socket reason: ~p~n", [Reason]))}
      end,
      ?MODULE ! stop,
      unregister(?MODULE)
  end.

listen(Port) ->
  case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]) of
    {ok, Socket} -> spawn(fun() -> accept(Socket) end);
    {error, Reason} -> stop(Reason)
  end.

accept(LSocket) ->
  case gen_tcp:accept(LSocket) of
    {ok, Socket} -> spawn(fun() -> accept(LSocket) end),
      loop(Socket);
    {error, Reason} ->
      stop(Reason)
  end.

loop(Sock) ->
  inet:setopts(Sock, [{active, once}]),
  receive
    {tcp, Socket, Data} ->
      NewConnection = erlang:term_to_binary({add, new}),
      case Data of
        NewConnection ->
          ?MODULE ! {add_info, "New connection ~n"},
          ?MODULE ! {add, Socket};
        _ -> ?MODULE ! {send, Data}
      end,
      loop(Socket);
    {tcp_closed, Socket} ->
      case whereis(?MODULE) of
        undefined -> ok;
        _ -> ?MODULE ! {remove, Socket}
      end;
    {tcp_error, Socket, Reason} ->
      ?MODULE ! {add_info,
        lists:flatten(
          io_lib:format("Error on socket ~p reason: ~p~n", [Socket, Reason]))}
  end.

server(Set, File) ->
  receive
    {add, Socket} ->
      NewSet = sets:add_element(Socket, Set),
      server(NewSet, File);
    {remove, Socket} ->
      NewSet = sets:del_element(Socket, Set),
      server(NewSet, File);
    {send, Message} ->
      sets:fold(fun(Socket, _) -> gen_tcp:send(Socket, Message) end, 0, Set),
      server(Set, File);
    {add_info, Message} ->
      file:write(File, Message),
      server(Set, File);
    stop ->
      Message = term_to_binary({server, stop}),
      sets:fold(fun(Socket, _) -> gen_tcp:send(Socket, Message) end, 0, Set),
      file:close(File)
  end.
