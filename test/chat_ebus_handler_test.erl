-module(chat_ebus_handler_test).

-include_lib("eunit/include/eunit.hrl").

handler_test() ->
    Res = chat_ebus_handler:handle_msg({none, <<"Type">>, <<"Msg">>}, self()),
    ?assertEqual({<<"Type">>, <<"Msg">>}, Res),
    Res1 = chat_ebus_handler:handle_msg({none, <<"Type">>, <<"Msg">>}, none),
    ?assertEqual(ok, Res1),
    Res2 = chat_ebus_handler:handle_msg(none, none),
    ?assertEqual(error, Res2).
