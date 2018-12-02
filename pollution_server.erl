-module(pollution_server).
-import(pollution,[createMonitor/0,addStation/3,addValue/5,removeValue/4,removeValue2/4,getOneValue/4,getStationMean/3,getDailyMean/3,getMaximumGrowthTime/2,simpleTest/0]).
-export([start_link/0,stop/0,loop/1,addStation/2,addValue/4,removeValue/3,crash/0,getOneValue/3,getStationMean/2,getDailyMean/2,init/0]).

-record(station,{coordinates,measurements}).
-record(measurement,{type,value,date}).


start_link() ->
  register(pollutionServer, spawn_link(pollution_server,init,[])).

stop() ->
  pollutionServer ! stop.

init()->
  NewMonitor = pollution:createMonitor(),
  loop(NewMonitor).
crash()->
  pollutionServer ! crash.

loop(Monitor)->
    receive
      stop -> ok;
      crash -> 1/0;
      {addStation,Name,Coordinates,ClientPid} ->
        case pollution:addStation(Name,Coordinates,Monitor) of
          {error,_} ->
                       ClientPid ! {reply,error},
                       loop(Monitor);
          NewMonior ->
                       ClientPid ! {reply,ok},
                       loop(NewMonior)
        end;

      {addValue,Station,Date,Type,Value,ClientPid} ->
        case pollution:addValue(Station,Date,Type,Value,Monitor) of
          {error,_} ->
                       ClientPid ! {reply,error},
                       loop(Monitor);
          NewMonior ->
                       ClientPid ! {reply,ok},
                       loop(NewMonior)
        end;

      {removeValue,Station,Date,Type,ClientPid} ->
        case pollution:removeValue(Station,Date,Type,Monitor) of
          {error,_} ->
                       ClientPid ! {reply,error},
                       loop(Monitor);
          NewMonior ->
                       ClientPid ! {reply,ok},
                       loop(NewMonior)
        end;

      {getOneValue,Station,Date,Type,ClientPid} ->
        case pollution:getOneValue(Station,Date,Type,Monitor) of
          {error,_} ->
                       ClientPid ! {reply,error},
                       loop(Monitor);
          Value      ->
                       ClientPid ! {reply,ok,Value},
                       loop(Monitor)
        end;

      {getStationMean,Station,Type,ClientPid} ->
        case pollution:getStationMean(Station,Type,Monitor) of
          {error,_} ->
                       ClientPid ! {reply,error},
                       loop(Monitor);
          Value ->
                       ClientPid ! {reply,ok,Value},
                       loop(Monitor)
        end;
      {getDailyMean,Date,Type,ClientPid} ->
        case pollution:getDailyMean(Date,Type,Monitor) of
          Value ->
                       ClientPid ! {reply,ok,Value},
                       loop(Monitor)
        end;
      {getMaximumGrowthTime,Type,ClientPid} ->
        case pollution:getMaximumGrowthTime(Type,Monitor) of
          Value ->
                       ClientPid ! {reply,ok,Value},
                       loop(Monitor);
          true -> 13
        end
    end.

    addStation(Name,Coordinates) ->
      pollutionServer ! {addStation,Name,Coordinates,self()},
      receive
        {reply,ok}    -> io:format("Adding station succesful! ~n"),
                         ok;
        {reply,error} -> io:format("Error while adding station! ~n"),
                          error
      end.

    addValue(Station,Date,Type,Value) ->
      pollutionServer ! {addValue,Station,Date,Type,Value,self()},
      receive
        {reply,ok}    -> io:format("Adding value succesful! ~n"),
                          ok;
        {reply,error} -> io:format("No such station! ~n"),
                          error
      end.

    removeValue(Station,Date,Type) ->
      pollutionServer ! {removeValue,Station,Date,Type,self()},
      receive
        {reply,ok}    -> io:format("Removing value succesful! ~n"),
                         ok;
        {reply,error} -> io:format("No such station! ~n"),
                         error
      end.

    getOneValue(Station,Date,Type) ->
      pollutionServer ! {getOneValue,Station,Date,Type,self()} ,
      receive
        {reply,ok,Value} -> io:format("Received value is: ~w ~n",[Value]),
                            Value;
        {reply,error}    -> io:format("No such station! ~n"),
                            error
      end.

    getStationMean(Station,Type) ->
      pollutionServer ! {getStationMean,Station,Type,self()},
      receive
        {reply,ok,Value} -> io:format("Received value is: ~w ~n",[Value]),
                            Value;
        {reply,error}    -> io:format("No such station! ~n"),
                            error
      end.

    getDailyMean(Date,Type) ->
      pollutionServer !   {getDailyMean,Date,Type,self()},
      receive
        {reply,ok,Value} -> io:format("Received value is: ~w ~n",[Value]),
                            Value
      end.

    getMaximumGrowthTime(Type) ->
      pollutionServer !   {getMaximumGrowthTime,Type,self()},
      receive
        {reply,ok,Value} -> io:format("Received value is: ~w ~n",[Value]),
                            13;
        _          -> 13
      end.
