-module(control).
-export([graphToNetwork/0]).
circularNetwork3 () ->
  [{red  , [{white, [white, blue]}]},
   {white, [{blue , [red, blue]}]},
   {blue , [{red  , [red, white]}]}
  ].
graphToNetwork() ->
    Graph = circularNetwork3(),
    Routers = ets:new(undef, [private]),
    ETable = ets:new(undef, [private]),
    % below code is for spwan routers
    lists:foreach(fun({NodeName, Edges}) ->
                         %% start routers
                         Pid = router:start(NodeName),
                         ets:insert(Routers, {NodeName, Pid}),
                         %% count outgoing edges of cur router
                         lists:foreach(fun({Dst, _}) -> 
                                              In = ets:lookup(ETable, Dst),
                                              case In == [] of
                                                  true ->
                                                      ets:insert(ETable, {Dst, 1});
                                                  false -> 
                                                      [{_, N}] = In,
                                                      ets:insert(ETable, {Dst, N+1})
                                              end
                                      end, Edges)
                 end, Graph),
    initRouters(Graph, Routers, ETable),
    Expected = length(Graph),
    AllPass = loop(Expected),
    case AllPass of
        true -> 
            [First|_] = Graph,
            {FirstName, _} = First,
            [{_, Rid}] = ets:lookup(Routers, FirstName),
            ets:delete(Routers),
            ets:delete(ETable),
            Rid;
        false -> 
            ets:delete(Routers),
            ets:delete(ETable),
            failed
        end.

loop(Expected) ->
    receive 
        {committed, _, 0} ->
            case Expected - 1 of
                0 -> true;
                _ -> loop(Expected - 1)
            end; 
        {abort, _, 0} -> false
    end.

    % List = ets:match_object(ETable, {'$0', '$1'}),
    % io:format("Router ~p~n", [List]).

initRouters(Graph, Routers, ETable) ->
    lists:foreach(fun({NodeName, Edges}) -> 
        [{_, N}] =  ets:lookup(ETable, NodeName),
        [{_, Pid}] = ets:lookup(Routers, NodeName),
        RT = initRoutingTable(Edges, Routers, []),
        Pid ! {control, self(), self(), 0,
            fun(_, Table) -> 
                ets:insert(Table, {'$NoInEdges', N}),
                ets:insert(Table, RT)
                end}
        end, Graph).

initRoutingTable([], _, RT) ->
    RT;
initRoutingTable([{Nei, Labels}|T], Routers, RT) ->
    [{_, Pid}] = ets:lookup(Routers, Nei),
    R = [{Dst, Pid} || Dst <- Labels],
    initRoutingTable(T, Routers, RT ++ R).



%% extendNetwork(RootPid, SeqNum, From, {NodeName, Edges}) ->
