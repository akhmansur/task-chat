-module(chat_ws_handler_test).
-include_lib("eunit/include/eunit.hrl").

parse_message_test() ->
  Message1 = <<"[{\"type\":\"username\"},{\"msg\":\"first_room\"}]">>,
  Message2 = <<"[{\"other\":\"username\"},{\"msg\":\"first_room\"}]">>,
  Message3 = <<"[\"username\"},{\"msg\":\"first_room\"}]">>,
  {ok, {Type, Msg}} = chat_ws_handler:parse_message(Message1),
  ?assertEqual(<<"username">>, Type),
  ?assertEqual(<<"first_room">>, Msg),
  {ok, {parse_error, ErrMess1}} = chat_ws_handler:parse_message(Message2),
  ?assertEqual("invalid message format", ErrMess1),
  {ok, {parse_error, ErrMess2}} = chat_ws_handler:parse_message(Message3),
  ?assertEqual("decode error", ErrMess2).

encode_message_test() ->
  Message = <<"{\"type\":\"username\",\"msg\":\"test\"}">>,
  {ok, Encoded} = chat_ws_handler:encode_message(<<"username">>, <<"test">>),
  ?assertEqual(Message, Encoded),
  {ok, {parse_error, Error}} = chat_ws_handler:encode_message({<<"username">>}, <<"test">>),
  ?assertEqual("encode error", Error).