-module(chat_manager_test).
-include_lib("eunit/include/eunit.hrl").

chat_manager_init_test() ->
  application:set_env(chat, rooms, ["first_room", "second_room","third_room","fourth_room","fifth_room"]),
  chat_manager:start_link(),
  [{rooms, List}] = application:get_all_env(chat),
  RoomsList = lists:map(fun(Elem) -> list_to_binary(Elem) end, List),
  S = chat_manager:status(),
  #{users := Users, rooms := StateRoomsList} = S,
  ?assertEqual(chat_manager, Users),
  ?assertEqual(StateRoomsList, RoomsList),
  chat_manager:stop().

chat_manager_store_username_test() ->
  Chrooms = ["first_room", "second_room","third_room","fourth_room","fifth_room"],
  application:set_env(chat, rooms, Chrooms),
  chat_manager:start_link(),
  {ok, User} = chat_manager:get_room_members("first_room"),
  ?assertEqual([], User),
  chat_manager:store_username("first_room", <<"Test">>),
  {ok, Error} = chat_manager:store_username("first_room", <<"Test">>),
  ?assertEqual(error, Error),
  {ok, [User1]} = chat_manager:get_room_members("first_room"),
  {ok, Empty} = chat_manager:get_room_members("room"),
  ?assertEqual([], Empty),
  ?assertEqual(<<"Test">>, User1),
  chat_manager:store_username("first_room", <<"Test1">>),
  {ok, User2} = chat_manager:get_room_members("first_room"),
  ?assertEqual(2, length(User2)),
  chat_manager:remove_room_member(<<"Test1">>),
  {ok, [User3]} = chat_manager:get_room_members("first_room"),
  ?assertEqual(<<"Test">>, User3),
  chat_manager:stop().

chat_manager_chroom_list_test() ->
  Chrooms = ["first_room", "second_room","third_room","fourth_room","fifth_room"],
  application:set_env(chat, rooms, Chrooms),
  chat_manager:start_link(),
  {ok, List} = chat_manager:chroom_list(),
  Rooms = lists:map(fun(Elem) -> binary_to_list(Elem) end, List),
  ?assertEqual(Chrooms, Rooms),
  chat_manager:stop().

chat_manager_messgs_test() ->
  Chrooms = ["first_room", "second_room","third_room","fourth_room","fifth_room"],
  application:set_env(chat, rooms, Chrooms),
  chat_manager:start_link(),
  {ok, Empty} = chat_manager:get_last_mess("first_room"),
  ?assertEqual([], Empty),
  chat_manager:recv_message("Test", "first_room", <<"message1">>),
  {ok, [{_, Messages}]} = chat_manager:get_last_mess("first_room"),
  ?assertEqual(<<"message1">>, Messages),
  chat_manager:recv_message("Test", "first_room", <<"message2">>),
  chat_manager:recv_message("Test", "first_room", <<"message3">>),
  {ok, Mess} = chat_manager:get_last_mess("first_room"),
  ?assertEqual(3, length(Mess)),
  chat_manager:stop().

prepare_list_test() ->
  {ok, List} = chat_manager:prepare_list([], 0, "Mess"),
  ?assertEqual(1, length(List)),
  {ok, LargeList} = chat_manager:prepare_list(lists:seq(1,50), 50, "Mess"),
  ?assertEqual(50, length(LargeList)).
