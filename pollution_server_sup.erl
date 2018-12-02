-module(pollution_server_sup).
-import(pollution_server,[start_link/0]).
-export([start/0,init/0,waitForSth/0,stop/0]).

-record(station,{coordinates,measurements}).
-record(measurement,{type,value,date}).


start() ->
  spawn(pollution_server_sup,init,[]).

stop() ->
  pollution_server_sup ! stop.

init()->
  process_flag(trap_exit,true),
  pollution_server:start_link(),
  waitForSth().

waitForSth() ->
  receive
     {'EXIT',_,_} -> init();
     stop         ->
                      pollution_server:stop(),
                      ok;
     _            -> waitForSth()
  end.
