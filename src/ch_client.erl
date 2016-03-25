%%%-------------------------------------------------------------------
%%% Created : 02. Март 2016 19:42
%%%-------------------------------------------------------------------
-module(ch_client).
-export([start/0]).

-include_lib("wx/include/wx.hrl").

-define(Connect, 150).
-define(Send, 151).
-define(Disconnect, 152).

start() -> spawn_link(fun() -> init() end).

init() ->
  wx:new(),
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Simple chat client", [{size, {400, 400}},
    {style, ?wxDEFAULT_FRAME_STYLE band (bnot (?wxMAXIMIZE_BOX bor ?wxRESIZE_BORDER))}]),
  Panel = wxPanel:new(Frame),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:addSpacer(MainSizer, 15),
  VSizer1 = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:addSpacer(VSizer1, 15),

  OutputText = wxTextCtrl:new(Panel, ?wxID_ANY,
    [{size, {250, 250}},
      {style, ?wxTE_MULTILINE}]),
  wxTextCtrl:setEditable(OutputText, false),
  wxSizer:add(VSizer1, OutputText, []),
  wxSizer:addSpacer(VSizer1, 25),
  InputText = wxTextCtrl:new(Panel, ?wxID_ANY,
    [{size, {250, 50}}, {style, ?wxTE_MULTILINE}]),
  wxSizer:add(VSizer1, InputText, []),
  wxSizer:addSpacer(VSizer1, 10),
  wxSizer:add(MainSizer, VSizer1, []),
  wxSizer:addSpacer(MainSizer, 20),

  VSizer2 = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:addSpacer(VSizer2, 15),
  BConnect = wxButton:new(Panel, ?Connect, [{label, "Connect"}, {size, {80, 30}}]),
  BDisconnect = wxButton:new(Panel, ?Disconnect, [{label, "Disconnect"}, {size, {80, 30}}]),
  BSend = wxButton:new(Panel, ?Send, [{label, "Send"}, {size, {80, 50}}]),
  NickText = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "NickName"}, {size, {80, 25}}]),
  Ip = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "127.0.0.1"}, {size, {80, 25}}]),
  Port = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "6268"}, {size, {80, 25}}]),
  wxSizer:add(VSizer2, BConnect, []),
  wxSizer:addSpacer(VSizer2, 10),
  wxSizer:add(VSizer2, Ip, []),
  wxSizer:addSpacer(VSizer2, 10),
  wxSizer:add(VSizer2, Port, []),
  wxSizer:addSpacer(VSizer2, 50),
  wxSizer:add(VSizer2, NickText, []),
  wxSizer:addSpacer(VSizer2, 10),
  wxSizer:add(VSizer2, BDisconnect),
  wxSizer:addSpacer(VSizer2, 60),
  wxSizer:add(VSizer2, BSend),
  wxSizer:add(MainSizer, VSizer2, []),
  wxPanel:setSizer(Panel, MainSizer),
  wxPanel:connect(Panel, command_button_clicked),
  wxFrame:connect(Frame, close_window),
  wxFrame:show(Frame),
  Pid = self(),
  loop([Frame, OutputText, InputText, BConnect], [NickText, Ip, Port], [], Pid).

loop([Frame, OutputText, InputText, B1] = Widgets, [NickText, Ip, Port] = Params, ClientSocket, Pid) ->
  receive

    {add_message, Answer} ->
      wxTextCtrl:appendText(OutputText, "\n" ++ Answer),
      loop(Widgets, Params, ClientSocket, Pid);

    {server,stop}->
      setEditable({Params,B1},true),
      showMessageBox(Frame,"Сервер отключен"),
      gen_tcp:close(ClientSocket),
      loop(Widgets, Params, [], Pid);

    #wx{id = ?Connect, event = #wxCommand{type = command_button_clicked}} ->
      Answer = client(
        iptotuple(wxTextCtrl:getValue(Ip)),
        get_port(wxTextCtrl:getValue(Port)),
        Pid),
      case Answer of
        {ok, Socket} ->
          setEditable({Params,B1},false),
          send(Socket, term_to_binary({add, new})),
          loop(Widgets, Params, Socket, Pid);
        {error, Reason} ->
          showMessageBox(Frame,
            "Невозможно установить соединение : " ++ atom_to_list(Reason)),
          loop(Widgets, Params, ClientSocket, Pid)
      end;

    #wx{id = ?Send, event = #wxCommand{type = command_button_clicked}} ->
      case wxWindow:isEnabled(B1) of
        false ->
          send(
            ClientSocket,
            create_message(
              wxTextCtrl:getValue(NickText),
              wxTextCtrl:getValue(InputText))),
          wxTextCtrl:setValue(InputText, "");
        true ->
          showMessageBox(Frame, "Вначале установите соединение!")
      end,
      loop(Widgets, Params, ClientSocket, Pid);

    #wx{id = ?Disconnect, event = #wxCommand{type = command_button_clicked}} ->
      case is_list(ClientSocket) of
        true -> ok;
        false -> disconnect(ClientSocket)
      end,
      setEditable({Params,B1},true),
      loop(Widgets, Params, [], Pid);

    #wx{event = #wxClose{}} ->
      case is_list(ClientSocket) of
        true -> ok;
        false -> disconnect(ClientSocket)
      end,
      wx:destroy()
  end.

client(Host, Port, Pid) ->
  case gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, 0}]) of
    {ok, Socket} -> spawn(fun() -> recv(Socket, Pid) end), {ok, Socket};
    Other -> Other
  end.

recv(Socket, Pid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      Temp=term_to_binary({server, stop}),
      case Data of
         Temp -> Pid ! {server, stop};
        _ -> Pid ! {add_message, Data}
      end,
      recv(Socket, Pid);
    {error, Reason} -> Reason
  end.

send(Socket, Message) ->
  gen_tcp:send(Socket, Message).

disconnect(Socket) ->
  gen_tcp:close(Socket).

create_message(Nick, Text) -> unicode:characters_to_binary(Nick ++ " >> " ++ Text).

iptotuple(Ip) ->element(2,inet:parse_address(Ip)).

get_port(Str) -> element(1, string:to_integer(Str)).

setEditable({TextControls,Button},State)->
  lists:foreach(fun(El)->wxTextCtrl:setEditable(El, State) end,TextControls),
  wxWindow:enable(Button, [{enable, State}]).

showMessageBox(Frame,Text)->
  MD = wxMessageDialog:new(Frame,Text, [{style, ?wxOK}, {caption, "Warning"}]),
  wxDialog:showModal(MD),
  wxDialog:destroy(MD).


