-module(router).
-export([start/1]).

start(RouterName) ->
    spawn(fun() -> init(RouterName) end).

init(RouterName) ->
    Rtable = ets:new(undef, [private]),
    Ctable = ets:new(undef, [private]),
    % ets:insert(Rtable, {'$NoInEdges', 0}).
    loop(RouterName, Rtable, Ctable).

loop(RouterName, Rtable, Ctable) ->
   receive
       {control, From, Pid, SeqNum, ControlFun} ->
           NRtable = handleControl(RouterName, Rtable, Ctable, From, Pid, SeqNum, ControlFun),
           loop(RouterName, NRtable, Ctable)
        
        end.

handleControl(RouterName, Rtable, _, _, Pid, 0, ControlFun) ->
    case ControlFun(RouterName, Rtable) of
        abort ->
            Pid ! {abort, self(), 0},
            exit(abort);
        _ -> 
            Pid ! {committed, self(), 0},
            Rtable
        end.

