-module(chat_manager).

-behaviour(gen_server).

%% API functions
-export([start_link/0
        ,stop/0
        ,store_username/2
        ,get_room_members/1
        ,remove_room_member/1
        ,chroom_list/0
        ,update_members_room/2
        ,recv_message/3
        ,get_last_mess/1
        ,status/0]).

-export([prepare_list/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stores username
%% @end
%%--------------------------------------------------------------------
store_username(RoomName, Username) ->
    gen_server:call(?MODULE, {store_username, RoomName, Username}).

%%--------------------------------------------------------------------
%% @doc
%% Returns list if roommates
%% @end
%%--------------------------------------------------------------------

get_room_members(RoomName) ->
    gen_server:call(?MODULE, {get_room_members, RoomName}).

%%--------------------------------------------------------------------
%% @doc
%% Returns last room messages
%% @end
%%--------------------------------------------------------------------

get_last_mess(RoomName) ->
    gen_server:call(?MODULE, {get_last_mess, RoomName}).

%%--------------------------------------------------------------------
%% @doc
%% Removes user from room
%% @end
%%--------------------------------------------------------------------

remove_room_member(RoomMember) ->
    gen_server:call(?MODULE, {remove_room_member, RoomMember}).

%%--------------------------------------------------------------------
%% @doc
%% Updates users room
%% @end
%%--------------------------------------------------------------------

update_members_room(RoomName, Username) ->
    gen_server:cast(?MODULE, {update_members_room, RoomName, Username}).

%%--------------------------------------------------------------------
%% @doc
%% Stores user message
%% @end
%%--------------------------------------------------------------------

recv_message(Name, RoomName, Msg1) ->
    gen_server:cast(?MODULE, {recv_message, Name, RoomName, Msg1}).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of rooms
%% @end
%%--------------------------------------------------------------------

chroom_list() ->
    gen_server:call(?MODULE, {chroom_list}).


status() ->
    gen_server:call(?MODULE, status).


stop() ->
    gen_server:cast(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), timeout()} |
                              ignore |
                              {stop, Reason :: term()}).
init([]) ->
    ets:new(?MODULE, [set, public, named_table
                     ,{write_concurrency, false}
                     ,{read_concurrency, true}]),
    ets:new(rooms, [set, public, named_table
                     ,{write_concurrency, false}
                     ,{read_concurrency, true}]),
    [{rooms, List}] = application:get_all_env(chat),
    RoomsList = lists:map(fun(Elem) -> list_to_binary(Elem) end, List),
    lists:foreach(fun(Elem) -> ets:insert(rooms, {Elem, []}) end, RoomsList),
    {ok, #{users => ?MODULE, rooms => RoomsList}}.

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: term()) ->
                                  {reply, Reply :: term(), State} |
                                  {reply, Reply :: term(), State, timeout()} |
                                  {noreply, State} |
                                  {noreply, State, timeout()} |
                                  {stop, Reason :: term(), Reply :: term(), State} |
                                  {stop, Reason :: term(), State}).
handle_call({store_username, BusName, Username}, _From, #{users := Users} = State) ->
    Reply = case ets:match(Users, {Username, '$1'}, 1) of
                '$end_of_table' ->
                    ets:insert(?MODULE, {Username, BusName}),
                    {ok, Username};
                _ ->
                    {ok, error}
            end,
    {reply, Reply, State};

handle_call({get_room_members, RoomName}, _From, #{users := Users} = State) ->
    Reply = case ets:match(Users, {'$1', RoomName}) of
                '$end_of_table' ->
                    {ok, []};
                List ->
                    {ok, lists:flatten(List)}
            end,
    {reply, Reply, State};

handle_call({get_last_mess, RoomName}, _From, State) ->
    Reply = case ets:match(rooms, {RoomName, '$1'}) of
                '$end_of_table' ->
                    {ok, []};
                List ->
                    {ok, lists:flatten(List)}
            end,
    {reply, Reply, State};

handle_call({remove_room_member, RoomMember}, _From, #{users := Users} = State) ->
    ets:delete(Users, RoomMember),
    {reply, ok, State};

handle_call({chroom_list}, _From,  #{rooms := RoomsList} = State) ->
    {reply, {ok, RoomsList}, State};

handle_call(status, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: term()) -> 
                                {noreply, State :: term()} |
                                {noreply, State :: term(), timeout()} |
                                {stop, Reason :: term(), State :: term()}).
handle_cast({update_members_room, BusName, Username}, #{users := Users} = State) ->
    true = ets:insert(Users, {Username, BusName}),
    {noreply, State};

handle_cast({recv_message, Name, RoomName, Msg}, State) ->
    {ok, RoomList} = case ets:match(rooms, {RoomName, '$1'}) of
                '$end_of_table' ->
                    {ok, []};
                List ->
                    {ok, lists:flatten(List)}
        end,
    {ok, RList} = prepare_list(RoomList, length(RoomList), 
                        [{Name, Msg}]),
    ets:insert(rooms, {RoomName, RList}),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout | term(), State :: term()) -> 
                            {noreply, State :: term()} |
                            {noreply, State :: term(), timeout()} |
                            {stop, Reason :: term(), State :: term()}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                                term()), State :: term()) -> no_return()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term(), State :: term(), Extra :: term()) -> 
    {ok, NewState ::term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Prepare messages list to insert in ets table
%%
%% @end
%%--------------------------------------------------------------------
-spec(prepare_list( RoomList :: list(), Length :: non_neg_integer(), 
                    Msg :: bitstring()) -> {ok, MessagesList :: list()}).
prepare_list(RoomList, Length, Msg) ->
    if 
        Length >= 50 -> 
            [_|List] = RoomList,
            {ok, List ++ [Msg]};
        true ->
            {ok, RoomList++[Msg]}
    end.













