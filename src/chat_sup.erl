%%%-------------------------------------------------------------------
%% @doc chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(Id, Mod, Args, Restart, Type), {Id, {Mod, start_link, Args},
                                              Restart, 60000, Type, [Mod]}).

-define(SIMPLE_CHILD(WorkerMod), ?CHILD(WorkerMod, WorkerMod, [], transient,
                                        worker)).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Whenever a supervisor is started using start_link/2,3, this function 
%% is called by the new process to find out about restart strategy, 
%% maximum restart intensity, and child specifications.
%% @end
%%--------------------------------------------------------------------

-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    {ok, { {one_for_all, 10, 60}, [?SIMPLE_CHILD(chat_manager)]} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
