-module(control).
-export([graphToNetwork/1, extendNetwork/4]).
% circularNetwork3 () ->
%   [{red  , [{white, [white, blue]}]},
%    {white, [{blue , [red, blue]}]},
%    {blue , [{red  , [red, white]}]}
%   ].
graphToNetwork(Graph) ->
    % Graph = circularNetwork3(),
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


initRoutingTableByid([], RT) ->
    RT;
initRoutingTableByid([{Nid, Labels}|T], RT) ->
    R = [{Dst, Nid} || Dst <- Labels],
    initRoutingTableByid(T, RT ++ R).

spawnFun(Table1, NodeName, NodeList, Cpid) ->
    Pid = router:start(NodeName),
    Pid ! {control, self(), Cpid, 0, 
        fun(_, Table2)->
            ets:insert(Table2, NodeList),
            ets:insert(Table2, {'$NoInEdges', 1})
        end},
    ets:insert(Table1, {NodeName, Pid}),
    Pid.

extendNetwork(RootPid, SeqNum, From, {NodeName, Edges}) ->
    NewNodeList = initRoutingTableByid(Edges, []),
    Cpid = self(),
    RootPid ! {control, self(), self(), SeqNum, 
        fun(Name, Table) ->
            case Name of
                From -> % spawn new process
                    try spawnFun(Table, NodeName, NewNodeList, Cpid) of
                        Pid ->[Pid]
                    catch
                        _:_ -> abort
                    end;
                _ ->
                    % if the node can get to 'From' then can get to the new node
                    Next = ets:lookup(Table, From),
                    case Next of
                        [{_, Nid}] ->
                            ets:insert(Table, {NodeName, Nid})
                    end,
                    % if a node has connection with the new node then update $NoInEdges
                    lists:foreach(
                        fun({In, _}) -> 
                            case self() of
                                In ->  
                                    [{_, N}] = ets:lookup(Table, '$NoInEdges'),
                                    ets:insert(Table, {'$NoInEdges', N+1})
                                end
                        
                        end, Edges),
                    [] % no processed spawned
            end
        end},
    receive
        {committed, _, SeqNum} ->
            true;
        {abort, _, SeqNum} ->
            false
    end.
    