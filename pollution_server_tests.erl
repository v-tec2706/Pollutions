-module(pollution_server_tests).
-include_lib("eunit/include/eunit.hrl").


addStationDoubly_test() ->
  pollution_server:start(),
   pollution_server:addStation("Krakowe", {150, 10}),
  P2 = pollution_server:addStation("Krakowe", {150, 10}),
  pollution_server:stop(),

  ?assertEqual(error, P2).

removeValueFromNotExistingStation_test() ->
  pollution_server:start(),
  pollution_server:addStation("Krakow", {10, 10}),
  P2 = pollution_server:removeValue("Alamakota",{{2018,04,11},{12,44,09}},"PM10"),
  pollution_server:stop(),

  ?assertEqual(error,P2).

getOneValue_test() ->
  pollution_server:start(),

  pollution_server:addStation("Krakowt", {1440, 10}),
  pollution_server:addValue("Krakowt",{{2018,04,11},{12,44,09}},"PM10",20),
  Val = pollution_server:getOneValue("Krakowt",{{2018,04,11},{12,44,09}},"PM10"),
  pollution_server:stop(),

  ?assertEqual(20,Val).

getStationMean_test() ->
  pollution_server:start(),

   pollution_server:addStation("Krako", {1, 10}),
  pollution_server:addStation("Katowice", {20, 20}),
  pollution_server:addValue("Krako",{{2018,04,11},{12,44,09}},"PM10",20),
  pollution_server:addValue("Krako",{{2018,05,10},{10,34,19}},"PM10",10),
  pollution_server:addValue("Katowice",{{2018,05,10},{10,34,19}},"PM10",10),
  Val = pollution_server:getStationMean("Krako","PM10"),
  pollution_server:stop(),

 ?assertEqual(15.0,Val).

getDailyMean_test() ->
  pollution_server:start(),

  pollution_server:addStation("Krak", {10, 1}),
  pollution_server:addStation("Katowic", {2, 20}),
  pollution_server:addValue("Krak",{{2018,04,10},{12,44,09}},"PM10",20),
  pollution_server:addValue("Krak",{{2018,04,10},{10,34,19}},"PM10",10),
  pollution_server:addValue("Katowic",{{2018,04,10},{12,44,09}},"PM10",30),
  Val = pollution_server:getDailyMean({2018,04,10},"PM10"),
  pollution_server:stop(),
 ?assertEqual(20.0,Val).

getMaximumGrowthTime_test() ->
  pollution_server:start(),

   P1=pollution_server:addStation("Orakow", {130, 10}),
   P2=pollution_server:addStation("Katowicer", {240, 20}),
   P3=pollution_server:addValue("Orakow",{{2018,04,10},{12,44,09}},"PM10",20),
   P4=pollution_server:addValue("Orakow",{{2018,04,10},{10,34,19}},"PM10",10),
   P15=pollution_server:addValue("Katowicer",{{2018,04,10},{13,44,09}},"PM10",50),
   P16 = pollution_server:addValue("Katowicer",{{2018,04,10},{14,44,09}},"PM10",45),
   Val = pollution_server:getMaximumGrowthTime("PM10"),
   %pollution_server:stop(),
  ?assertEqual(13,Val).
