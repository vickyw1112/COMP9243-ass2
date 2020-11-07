-module(router).
-export([start/1]).

start(RouterName) ->
    spawn(fun() -> init(RouterName) end).

init(RouterName) ->
    Rtable = ets:new(undef, [private]),
    loop(RouterName, Rtable, []).

loop(RouterName, Rtable, Seqtable) ->
   receive
        {message, Dest, From, Pid, Trace} ->
            NewTrace = [RouterName|Trace],
            case RouterName of
                Dest -> 
                    Pid ! {trace, self(), lists:reverse(NewTrace)};
                _ ->
                    Next = ets:lookup(Rtable, Dest),
                    % TODO change here
                    case Next of
                        [] -> io:format("something bad happend~n");
                        [{_, Nid}] -> 
                            Nid ! {message, Dest, self(), Pid, NewTrace}
                    end
            end,
            loop(RouterName, Rtable, Seqtable);
        {control, From, Pid, SeqNum, ControlFun} when SeqNum == 0 ->
            case ControlFun(RouterName, Rtable) of
                abort ->
                    Pid ! {abort, self(), 0},
                    exit(abort);
                _ -> 
                    Pid ! {committed, self(), 0},
                    loop(RouterName, Rtable, Seqtable)
            end;
        {control, From, Pid, SeqNum, ControlFun} when SeqNum /= 0 ->
           NRtable = handleControl(RouterName, Rtable, Seqtable, From, Pid, SeqNum, ControlFun),
           loop(RouterName, NRtable, Seqtable);
        {dump, From} ->
            Dump = ets:match(Rtable, '$1'),
            From ! {table, self(), Dump},
            loop(RouterName, Rtable, Seqtable);
        stop ->
            ets:delete(Rtable),
            clearMailbox(),
            exit(stop);
        _ -> 
            loop(RouterName, Rtable, Seqtable) 
        end.

clearMailbox() ->
    receive
        _Any ->
            clearMailbox()
        after 0 ->
            ok
        end.

handleControl(RouterName, Rtable, Seqtable, From, Pid, SeqNum, ControlFun) ->
    NewSeq = [SeqNum|Seqtable],
    Dump = lists:flatten(ets:match(Rtable, '$1')),
    TempTable = ets:new(undef, [private]),
    ets:insert(TempTable, Dump),
    case Pid of
        % this is root node 
        From -> 
            broadcast(Rtable, {control, self(), Pid, SeqNum, ControlFun}),
            Res = handleControlFun(RouterName, TempTable, ControlFun),
            Replys = coordinate1(Rtable, SeqNum, NewSeq),
            case Res of
                abort ->
                    % here broadcast phase1 result to everyone
                    broadcast(Rtable, {doAbort, self(), SeqNum}),
                    Pid ! {abort, self(), SeqNum},
                    ets:delete(TempTable),
                    Rtable;
                Children when Replys == abort ->
                    broadcast(Rtable, {doAbort, self(), SeqNum}),
                    lists:foreach(fun(C) -> 
                            exit(C, abort)
                        end, Children),
                    Pid ! {abort, self(), SeqNum},
                    ets:delete(TempTable),
                    Rtable;
                Children when Replys == yes ->
                    broadcast(Rtable, {doCommit, self(), SeqNum}),
                    Pid ! {committed, self(), SeqNum},
                    ets:delete(Rtable),
                    TempTable
            end;
        % normal nodes
        _Else ->
            broadcast(Rtable, {control, self(), Pid, SeqNum, ControlFun}),
            case handleControlFun(RouterName, TempTable, ControlFun) of
                abort ->
                    % phase1
                    coordinate1(Rtable, SeqNum, NewSeq),
                    From ! {abort, self(), SeqNum},
                    % phase2 
                    % Replys is not important here
                    coordinate2(SeqNum),
                    broadcast(Rtable, {doAbort, self(), SeqNum}),
                    ets:delete(TempTable),
                    Rtable;
                Children ->
                    Reply1 = coordinate1(Rtable, SeqNum, NewSeq),
                    case Reply1 of
                        abort -> From ! {abort, self(), SeqNum};
                        yes -> From ! {yes, self(), SeqNum}
                    end,
                    Reply2 = coordinate2(SeqNum),
                    broadcast(Rtable, {Reply2, self(), SeqNum}),
                    case Reply2 of
                        doAbort ->
                            ets:delete(TempTable),
                            lists:foreach(fun(C) -> 
                                    exit(C, abort)
                                end, Children),
                            Rtable;
                        doCommit ->
                            ets:delete(Rtable),
                            TempTable
                    end
            end
    end.

coordinate2(SeqNum) ->
    receive 
        {doAbort, From, SeqNum} ->
            doAbort;
        {doCommit, From, SeqNum} ->
            doCommit;
        _ -> coordinate2(SeqNum)
    end.

coordinate1(Rtable, SeqNum, Seqtable) ->
    Neis = [Nei || [{Dst, Nei}] <- ets:match(Rtable, '$1'), Dst =/= '$NoInEdges'] ,
    NodeCount = length(lists:usort(Neis)),
    phase1Loop(SeqNum, Seqtable, NodeCount).

phase1Loop(SeqNum, Seqtable, NodeCount) ->
    receive
        {abort, _, SeqNum} -> 
            % consume msg, wait until timeout to abort
            case NodeCount - 1 of
                0 -> abort;
                _ -> phase1Loop(SeqNum, Seqtable, NodeCount - 1)
            end;
        {yes, _, SeqNum} -> 
            case NodeCount - 1 of
                0 -> yes;
                _ -> phase1Loop(SeqNum, Seqtable, NodeCount - 1)
            end;
        {control, From, _, SeqNum, _} ->
            From ! {yes, self(), SeqNum},
            phase1Loop(SeqNum, Seqtable, NodeCount);
        % abort both control msg
        {control, From, _, SeqNum1, _} ->
            Doesfind = lists:member(SeqNum1, Seqtable),
            case Doesfind of
                true -> doNothing;
                _Else -> 
                    From ! {abort, self(), SeqNum1}
            end,
            abort;
        % abort both control msg
        {_, From, SeqNum1} -> 
            Doesfind = lists:member(SeqNum1, Seqtable),
            case Doesfind of
                true -> doNothing;
                _Else -> 
                    From ! {abort, self(), SeqNum1}
            end,
            abort
    after timeout() -> abort
    end.

handleControlFun(RouterName, Rtable, ControlFun) ->
    Children = ControlFun(RouterName, Rtable),
    case Children of
        abort ->
            abort;
        _ ->
            Children
    end.

broadcast(Rtable, Msg) ->
    Neis = [Nei || [{Dst, Nei}] <- ets:match(Rtable, '$1'), Dst =/= '$NoInEdges'] ,
    UniNeis = lists:usort(Neis),
    lists:foreach(
        fun(Nid) ->
            Nid ! Msg
        end, UniNeis).

timeout () -> 5000.