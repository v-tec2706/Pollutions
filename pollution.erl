-module(pollution).
-export([createMonitor/0,addStation/3,addValue/5,removeValue/4,removeValue2/4,getOneValue/4,getStationMean/3,getDailyMean/3,getMaximumGrowthTime/2,simpleTest/0]).

%-record(monitor,{stations}).
-record(station,{coordinates,measurements}).
-record(measurement,{type,value,date}).

simpleTest()->
    P = pollution:createMonitor(),
    P1 = pollution:addStation("Krakow", {10, 10}, P),
    P2 = pollution:addStation("Katowice", {20, 20}, P1),
    P3 = pollution:addValue("Krakow",{{2018,04,10},{12,44,09}},"PM10",20,P2),
    P4 = pollution:addValue("Krakow",{{2018,05,10},{10,34,19}},"PM10",10,P3),
    P5 = pollution:addValue("Katowice",{{2018,05,10},{10,34,19}},"PM10",10,P4).


createMonitor() ->
  Monitor = {maps:new(),maps:new()}.

addStation(Name,Coordinates, Monitor) ->
    {Stations,StationsCoor} = Monitor,
        case maps:is_key(Name,Stations) == false andalso maps:is_key(Coordinates,StationsCoor) == false of
              true ->
                NewStation = maps:put(Name,{Coordinates,[]},Stations),
                NewStationC = maps:put(Coordinates,Name,StationsCoor),
                NewMonitor = {NewStation,NewStationC};
              false -> {error,"Station with this attributes already exists!"}
        end.

findStationName(Monitor,Identifier)-> % takes station name or coordinates, returns name or error if no such station exists
  {Stations,StationsCoor} = Monitor,
  case maps:is_key(Identifier,Stations) == true of
    true -> Identifier;
    false ->
       case maps:is_key(Identifier,StationsCoor) == true of
          true ->  maps:get(Identifier,StationsCoor);
          false -> error
       end
   end.


addValue(Station,Date,Type,Value,Monitor) ->
{Stations,StationsCoor} = Monitor,

Name = findStationName(Monitor,Station),
case Name == error of
    false ->
      NewMeasurement =  #measurement{type=Type,value =Value,date = Date},
      {TakenCoordiantes,TakenMeasurements} = maps:get(Name,Stations),
      UpdatedStation = maps:update(Name,{TakenCoordiantes,[NewMeasurement|TakenMeasurements]},Stations),
      UpdatedMonitor = {UpdatedStation,StationsCoor};
    true  ->  {error,"No such station"}
end.


removeValuePred([],_,_) -> [];
removeValuePred([Elem|Tail],Date,Type) ->
  {_,ElemType,_,ElemDate} = Elem,
  case ElemType == Type andalso  ElemDate == Date of
    false -> [Elem|removeValuePred(Tail,Date,Type)];
    true -> removeValuePred(Tail,Date,Type)
end.

removeValue(Station,Date,Type,Monitor) ->

  {Stations,StationsCoor} = Monitor,
%  Name = findStationName(Monitor,Station),
%  case Name == error of
%      false ->
%        {TakenCoordiantes,TakenMeasurements} = maps:get(Name,Stations),
%        UpdatedList = removeValuePred(TakenMeasurements,Date,Type),
%        UpdatedStation = maps:update(Name,{TakenCoordiantes,TakenMeasurements},Stations),
%        UpdatedMonitor = {UpdatedStation,StationsCoor};

%      true  ->  {error,"No such station"}
%  end.



     case maps:is_key(Station,Stations) == true of
        true -> Name = Station,
       {TakenCoordiantes,TakenMeasurements} = maps:get(Name,Stations),
        UpdatedList = removeValuePred(TakenMeasurements,Date,Type),
        UpdatedStation = maps:update(Name,{TakenCoordiantes,UpdatedList},Stations),
      UpdatedMonitor = {UpdatedStation,StationsCoor};

      false ->
          case maps:is_key(Station,StationsCoor) == true of
            true ->  Name = maps:get(Station,StationsCoor),

                {TakenCoordiantes,TakenMeasurements} = maps:get(Name,Stations),
                UpdatedList = removeValuePred(TakenMeasurements,Date,Type),
                UpdatedStation = maps:update(Name,{TakenCoordiantes,TakenMeasurements},Stations),
                UpdatedMonitor = {UpdatedStation,StationsCoor};

            false -> {error, "No such station"}
          end
      end.

    removeValue2(Station,Date,Type,Monitor) ->

      {Stations,StationsCoor} = Monitor,
      Name = findStationName(Monitor,Station),
      case Name == error of
          false ->
            {TakenCoordiantes,TakenMeasurements} = maps:get(Name,Stations),
            UpdatedList = removeValuePred(TakenMeasurements,Date,Type),
            UpdatedStation = maps:update(Name,{TakenCoordiantes,TakenMeasurements},Stations),
            UpdatedMonitor = {UpdatedStation,StationsCoor};
          true  ->  {error,"No such station"}
      end.



getOneValuePred([],_,_) -> [];
getOneValuePred([Elem|Tail],Date,Type) ->
  {_,ElemType,ElemVal,ElemDate} = Elem,
  case ElemType == Type andalso  ElemDate == Date of
    true -> ElemVal;
    false -> getOneValuePred(Tail,Date,Type)
end.

getOneValue(Station,Date,Type,Monitor) ->
  {Stations,StationsCoor} = Monitor,

  Name = findStationName(Monitor,Station),
  case Name == error of
      false ->
        {TakenCoordiantes,TakenMeasurements} = maps:get(Name,Stations),
         getOneValuePred(TakenMeasurements,Date,Type);

      true  ->  {error,"No such station"}
  end.


  getStationMeanPred([],_,_,0) -> [];
  getStationMeanPred([],_,Sum,Count) -> Sum/Count;
  getStationMeanPred([Elem|Tail],Type,Sum,Count) ->
    {_,ElemType,ElemVal,ElemDate} = Elem,
    case ElemType == Type of
      true -> getStationMeanPred(Tail,Type,Sum+ElemVal,Count+1);
      false -> getStationMeanPred(Tail,Type,Sum,Count)
  end.

  getStationMean(Station,Type,Monitor) ->

    {Stations,StationsCoor} = Monitor,
    Name = findStationName(Monitor,Station),
    case Name == error of
        false ->
          {TakenCoordiantes,TakenMeasurements} = maps:get(Name,Stations),
          getStationMeanPred(TakenMeasurements,Type,0,0);
        true  ->  {error,"No such station"}
    end.


        extractValueFromMeasurements([],Date,Type,Sum,Count) -> {Sum,Count};
        extractValueFromMeasurements([Measurement|T],Date,Type,Sum,Count) ->
        {Year,Month,Day} = Date,
        {_,TakenType,Value,{{TakenYear,TakenMonth,TakenDay},_}} = Measurement,
        case Year == TakenYear andalso Month == TakenMonth andalso Day == TakenDay andalso Type == TakenType  of
          true-> extractValueFromMeasurements(T,Date,Type,Sum+Value,Count+1);
          false-> extractValueFromMeasurements(T,Date,Type,Sum,Count)
        end.


      extractMeasurements([],_,_,_,0) -> [];
      extractMeasurements([],_,_,Sum,Count) -> Sum/Count;
      extractMeasurements([H|T],Date,Type,Sum,Count) ->
        {_,{_,Measurements}} = H,
        {ReceivedVal,ReceivedCnt} = extractValueFromMeasurements(Measurements,Date,Type,0,0),
        extractMeasurements(T,Date,Type,Sum+ReceivedVal,Count+ReceivedCnt).

      getDailyMean(Date,Type,Monitor)->
          {Stations,StationsCoor} = Monitor,
          List = maps:to_list(Stations),
          extractMeasurements(List,Date,Type,0,0).


        extractValueFromMeasurements2([],_,List) -> List;
        extractValueFromMeasurements2([Measurement|T],Type,List) ->
          {_,TakenType,Value,{_,{TakenHour,_,_}}} = Measurement,
          case Type == TakenType of
            true -> extractValueFromMeasurements2(T,Type, [{Value,TakenHour} | List]);
            false -> extractValueFromMeasurements2(T,Type,List)
          end.

        extractMeasurements2([],_,List) -> List;
        extractMeasurements2([H|T],Type,List) ->
            {_,{_,Measurements}} = H,
            NewList = extractValueFromMeasurements2(Measurements,Type,[]),
            extractMeasurements2(T,Type,[NewList | List]).


          findMaxDif([{_,_}],{_,H}) -> H;
          findMaxDif([H|T],{Max,Hour}) ->
            {V1,H1} = H,
            [{V2,H2}|_] = T,
            case abs(V2-V1) > Max of
              true -> findMaxDif(T,{abs(V2-V1),H2});
              false -> findMaxDif(T,{Max,Hour})
            end.

        getMaximumGrowthTime(Type,Monitor) ->
          {Stations,_} = Monitor,
          List = maps:to_list(Stations),
          ValuesList = extractMeasurements2(List,Type,[]),
          B=lists:flatten(ValuesList),
          SortedValuesList = lists:sort(fun({KeyA,ValA}, {KeyB,ValB}) -> {ValA,KeyA} =< {ValB,KeyB} end,B),
         findMaxDif(SortedValuesList,{0,0}).
