-module(pollution_supervisor).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(InitValue) ->
  supervisor:start_link({local,varSupervisor},?MODULE,InitValue).


init(InitValue) ->
  {ok,{
    {one_for_all,2,3},
    [ {pollution_gen_server,
      {pollution_gen_server,start_link,[InitValue]},
      permanent,brutal_kill,worker,[pollution_server]}
    ]}
}.
