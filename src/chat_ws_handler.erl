
%%%-------------------------------------------------------------------
%%% @author akhmansur
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Websocket handler
%%% @end
%%%-------------------------------------------------------------------
-module(chat_ws_handler).

-export([init/2, 
    websocket_init/1,
    websocket_info/2,
    websocket_handle/2,
    websocket_terminate/3]).

-export([parse_message/1,
        encode_message/2]).

-define(CHATROOM_NAME, ?MODULE).
%% innactivity timeout
-define(TIMEOUT, 5 * 60 * 1000).

-record(state, {name, handler, room, wanted_room}).

%%%=======================================================================
%%% Callback Functions
%%%=======================================================================

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.


websocket_init(_State) ->
    Handler = ebus_proc:spawn_handler(fun chat_ebus_handler:handle_msg/2, [self()]),
    ebus:sub(Handler, default),
    {ok, #state{name = unknown, handler=Handler, room = default, wanted_room = default}}.

websocket_handle({text, Msg}, #state{name = _Name,
                                    handler = Handler,
                                    room = RoomName,
                                    wanted_room = NewRoomName} =  State) when RoomName =/= NewRoomName -> 
    {ok, {Type, Msg1}} = parse_message(Msg),
    case Type of
        <<"username">> ->
            %% check if username is assignable
            case chat_manager:store_username(NewRoomName, Msg1) of
                {ok, error} ->
                    {ok, Reply} = encode_message(<<"username_error">>, error),
                    {reply, {text, Reply}, State};
                _ ->
                    {ok, ChList}  = chat_manager:chroom_list(),
                    case lists:search(fun(Elem) -> Elem =:= NewRoomName end, ChList) of
                        {value, _} ->
                            ok          = ebus:unsub(Handler ,RoomName),
                            ok          = ebus:sub(Handler, NewRoomName),
                            %% signal chat_manager to update user room
                            chat_manager:update_members_room(NewRoomName, Msg1),
                            {ok, Messgs} = chat_manager:get_last_mess(NewRoomName),
                            LastMess = lists:map(fun(Elem) ->  [{[Elem]}]
                                end, Messgs),
                            %% send message to client last messages
                            {ok, Reply} = case LastMess of
                                [] ->
                                    encode_message(<<"last_messages">>, <<"">>);
                                _ ->
                                    encode_message(<<"last_messages">>, LastMess)
                            end,
                            {ok, List} = chat_manager:get_room_members(NewRoomName),
                            ok = ebus:pub(NewRoomName, {none, <<"room_members_list">>, List}),
                            {reply, {text, Reply}, State#state{room = NewRoomName, name = Msg1}};
                        false ->
                            {reply, {text, encode_message(<<"error">>, <<"room_not_exist">>)}, State}
                    end
            end;
        parse_error ->
            {ok, Reply} = encode_message(<<"error">>, <<"invalid message format">>),
            {reply, {text, Reply}, State};

        _ ->
            {ok, Reply} = encode_message(<<"error">>, <<"send your name">>),
            {reply, {text, Reply}, State}
    end;
        

websocket_handle({text, Msg}, #state{name = Name,
                                    handler = Handler,
                                    room = RoomName,
                                    wanted_room = RoomName} =  State) ->
    {ok, {Type, Msg1}} = parse_message(Msg),
    case Type of
        <<"chat">> ->
            ok = ebus:pub(RoomName, {self(),Name, Msg1}),
            chat_manager:recv_message(Name, RoomName, Msg1),
            {ok, State};

        <<"chroom_list">> ->
            {ok, List}  = chat_manager:chroom_list(),
            {ok, Reply} = encode_message(<<"chroom_list">>, List),
            {reply, {text, Reply}, State};

        <<"room_members_list">> ->
            case RoomName of
                default ->
                    List = <<"">>;
                _ ->
            		{ok, List}  = chat_manager:get_room_members(RoomName)
            end,
            {ok, Reply} = encode_message(<<"room_members_list">>, List),
            {reply, {text, Reply}, State};

        <<"enter_room">> ->
            {ok, Reply} = encode_message(<<"enter_username">>, Msg1),
            {reply, {text, Reply}, State#state{wanted_room = Msg1}};

        <<"username">> ->
            %% check if username is assignable
            case chat_manager:store_username(RoomName, Msg1) of
                {ok, error} ->
                    {ok, Reply} = encode_message(<<"username_error">>, error),
                    {reply, {text, Reply}, State};
                _ ->
                    {ok, List}  = chat_manager:chroom_list(),
                    {ok, Reply} = encode_message(<<"chroom_list">>, List),
                    {reply, {text, Reply}, State#state{name = Msg1}}
            end;

        <<"terminate">> ->
            chat_manager:remove_room_member(Name),
            ebus:unsub(RoomName, Handler),
            ebus_handler:delete(Handler),
            {ok, List} = chat_manager:get_room_members(RoomName),
            ok = ebus:pub(RoomName, {none, <<"room_members_list">>, List}),
            {shutdown, State};
        
        parse_error ->
            {ok, Reply} = encode_message(<<"error">>, <<"invalid message format">>),
            {reply, {text, Reply}, State};

        _ ->
            {ok, Reply} = encode_message(<<"error">>, <<"unknown message type">>),
            {reply, {text, Reply}, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({Type, Msg}, State) ->
%%    {ok, Reply} = encode_message(<<"message">>, {[{Type,Msg}]}),
    {ok, Reply} = encode_message(Type,Msg),
    {reply, {text, Reply}, State};

websocket_info(_Info, State) ->
    {ok, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.  

%%%=======================================================================
%%% Internal
%%%=======================================================================

%%--------------------------------------------------------------------
%% @doc
%% Decodes message from client
%% @end
%%--------------------------------------------------------------------
-spec(parse_message(Msg :: list()) -> {ok, {Type :: bitstring(), Msg :: bitstring()}} |
                                      {ok, {parse_error, ErrorMsg :: list()}}).
parse_message(Msg) ->
    try jiffy:decode(Msg) of
        Succ -> 
            Msg1 = lists:map(fun({[Elem]}) -> Elem end, Succ),
            case Msg1 of
                [{<<"type">>, Type}, {<<"msg">>,  Content}] -> 
                    {ok, {Type, Content}};
                _ -> 
                    {ok, {parse_error, "invalid message format"}}
            end
    catch 
        {error, _Reason} ->
            {ok, {parse_error, "decode error"}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Encodes reply
%% @end
%%--------------------------------------------------------------------
-spec(encode_message(Type :: bitstring(), Msg :: tuple() | list()) -> 
                                    {ok, Message :: bitstring()} |
                                    {ok, {parse_error, ErrorMsg :: list()}}).
encode_message(Type, Msg) ->
    Reply = {[{type, Type}, {msg, Msg}]},
    try jiffy:encode(Reply) of
        Succ -> 
            {ok, Succ}
    catch 
        {error, _Reason} ->
            {ok, {parse_error, "encode error"}}
    end.
