%%%-------------------------------------------------------------------
%% @doc ErlBus handler
%% @end
%%%-------------------------------------------------------------------
-module('chat_ebus_handler').
%% API
-export([handle_msg/2]).

-spec(handle_msg({Sender :: none, pid(), Type :: bitstring(), Msg :: bitstring()},
        User :: pid()) -> ok | {Type :: bitstring(), Msg :: bitstring()}).
handle_msg({Sender, Type, Msg}, User) ->
    if
        Sender =:= User -> ok;
        true            -> 
            User ! {Type, Msg}
    end;

handle_msg(_Data,_User) ->
    error.
