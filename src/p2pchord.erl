%%%-------------------------------------------------------------------
%%% @author Sripriya Simhadri - Neha Komuravelly
%%% @copyright (C) 2022, DOSP COP5615
%%% @doc
%%%
%%% @end
%%% Created : 19. Oct 2022 11:57 PM
%%%-------------------------------------------------------------------
-module(p2pchord).
-author("simkom").
-export([network_establish/2, spawningnode/4, monitor_tsk_done/2, start/2]).

deliver_msg_node(_, [], _) ->
  ok;
deliver_msg_node(H, NodesCreated, StateOfNetwork) ->
  [One | Extra] = NodesCreated,
  ProcID = retrieve_procid_node(One, StateOfNetwork),
  ProcID ! {browse, One, H, 0, self()},
  deliver_msg_node(H, Extra, StateOfNetwork).


nodes_kill_all([], _) ->
  ok;
nodes_kill_all(NodesCreatedList, StateOfNetwork) ->
  [One | Extra] = NodesCreatedList,
  retrieve_procid_node(One, StateOfNetwork) ! {kill},
  nodes_kill_all(Extra, StateOfNetwork).

start(NumberOfNodes, NumberOfReq) ->
  register(start_process, spawn(p2pchord, network_establish, [NumberOfNodes, NumberOfReq])).


arbitaryNode(IDOfNode, []) -> IDOfNode;
arbitaryNode(_, CurrentNode) -> lists:nth(rand:uniform(length(CurrentNode)), CurrentNode).


return_nearby(_, [], SmallestN, _, _) ->
  SmallestN;
return_nearby(H, IDOfFingerN, SmallestN, MinVal, State) ->
  [One| Extra] = IDOfFingerN,
  DistNew = request_onward_dist(H, One, dict:fetch(m, State), 0),
  if
    DistNew < MinVal ->
      return_nearby(H, Extra, One, DistNew, State);
    true ->
      return_nearby(H, Extra, SmallestN, MinVal, State)
  end.


request_onward_dist(H, H, _, DistNew) ->
  DistNew;
request_onward_dist(H, IDOfNode, LogN, DistNew) ->
  request_onward_dist(H, (IDOfNode + 1) rem trunc(math:pow(2, LogN)), LogN, DistNew + 1).


spawningnode(KeyHash, LogN, NodesCreatedList, StateOfNode) ->
  FngrTbl = lists:duplicate(LogN, arbitaryNode(KeyHash, NodesCreatedList)),
  StateOfNodeUpdated = dict:from_list([{id, KeyHash}, {predecessor, nil}, {fingertbl, FngrTbl}, {next, 0}, {m, LogN}]),
  listenN(StateOfNodeUpdated).


return_nearby_node(H, IDOfFingerN, St) ->
  case lists:member(H, IDOfFingerN) of
    true -> H;
    _ -> return_nearby(H, IDOfFingerN, -1, 10000000, St)
  end.


listenN(StateOfNode) ->
  KeyHash = dict:fetch(id, StateOfNode),
  receive
    {fingertbl, FngrTbl} ->
      NewSt = dict:store(fingertbl, FngrTbl, StateOfNode);

    {browse, S, H, HCnt, ProcID} ->

      NodeVal = return_nearby_node(H, dict:fetch_keys(dict:fetch(fingertbl ,StateOfNode)), StateOfNode),
      NewSt = StateOfNode,
      if

        (KeyHash == H) ->
          taskcompcheck ! {done, KeyHash, HCnt, H};
        (NodeVal == H) and (KeyHash =/= H) ->
          taskcompcheck ! {done, KeyHash, HCnt, H};

        true ->
          dict:fetch(NodeVal, dict:fetch(fingertbl, StateOfNode)) ! {browse, S, H, HCnt + 1, self()}
      end
  ;
    {kill} ->
      NewSt = StateOfNode,
      exit("Exiting!");
    {state, ProcID} -> ProcID ! StateOfNode,
      NewSt = StateOfNode;
    {equilibrate, StateOfNetwork} -> io:fwrite("~nEquilibrating P2P system!"),
      NewSt = StateOfNode
  end,
  listenN(NewSt).


retrieve_procid_node(KeyHash, StateOfNetwork) ->
  case dict:find(KeyHash, StateOfNetwork) of
    error -> nil;
    _ -> dict:fetch(KeyHash, StateOfNetwork)
  end.

monitor_tsk_done(0, HCnt) ->
  start_process ! {totH, HCnt}
;

monitor_tsk_done(NumberOfReq, HCnt) ->
  receive
    {done, ProcID, HCntForTask, H} ->
      monitor_tsk_done(NumberOfReq - 1, HCnt + HCntForTask)
  end.


deliver_msg_node_all(_, 0, _, _) ->
  ok;
deliver_msg_node_all(NodesCreatedList, NumberOfReq, M, StateOfNetwork) ->
  timer:sleep(1000),
  Key = lists:nth(rand:uniform(length(NodesCreatedList)), NodesCreatedList),
  deliver_msg_node(Key, NodesCreatedList, StateOfNetwork),
  deliver_msg_node_all(NodesCreatedList, NumberOfReq - 1, M, StateOfNetwork).


gettotH() ->
  receive
    {totH, HCnt} ->
      HCnt
  end.

network_establish(NumberOfNodes, NumberOfReq) ->
  LogN = get_log(NumberOfNodes),
  [NodesCreatedList, StateOfNetwork] = generate_nodes([], round(math:pow(2, LogN)), LogN, NumberOfNodes, dict:new()),
  ftable:circulate_fingertbl(StateOfNetwork,LogN),
  equilibrate(NodesCreatedList, StateOfNetwork),
  forward_msg_kill(NodesCreatedList, NumberOfNodes, NumberOfReq, LogN, StateOfNetwork).

get_log(NumberOfNodes) ->
  trunc(math:ceil(math:log2(NumberOfNodes))).


generate_nodes(NodesCreatedList, _, _, 0, StateOfNetwork) ->
  [NodesCreatedList, StateOfNetwork];
generate_nodes(NodesCreatedList, TotalNumberOfNodes, LogN, NumberOfNodes, StateOfNetwork) ->
  [KeyHash, NewStateOfNetwork] = attach_node(NodesCreatedList, TotalNumberOfNodes,  LogN, StateOfNetwork),
  generate_nodes(lists:append(NodesCreatedList, [KeyHash]), TotalNumberOfNodes, LogN, NumberOfNodes - 1, NewStateOfNetwork).


equilibrate(NodesCreatedList, StateOfNetwork) ->
  ProcID = retrieve_procid_node(lists:nth(rand:uniform(length(NodesCreatedList)), NodesCreatedList), StateOfNetwork),
  case ProcID of
    nil -> equilibrate(NodesCreatedList, StateOfNetwork);
    _ -> ProcID ! {equilibrate, StateOfNetwork}
  end,
  io:fwrite("~nEquilibrating P2P system!").


forward_msg_kill(NodesCreatedList, NumberOfNodes, NumberOfReq, LogN, StateOfNetwork) ->
  register(taskcompcheck, spawn(p2pchord, monitor_tsk_done, [NumberOfNodes * NumberOfReq, 0])),
  deliver_msg_node_all(NodesCreatedList, NumberOfReq, LogN, StateOfNetwork),
  TotH = gettotH(),
  io:format("~n Total Hops = ~p Number of nodes*requests = ~p So, Average Hops = ~p ~n", [TotH, NumberOfNodes * NumberOfReq, TotH/(NumberOfNodes * NumberOfReq)]),
  nodes_kill_all(NodesCreatedList, StateOfNetwork).


attach_node(NodesCreatedList, TotalNumberOfNodes, LogN, StateOfNetwork) ->
  ExtraHash = lists:seq(0, TotalNumberOfNodes - 1, 1) -- NodesCreatedList,
  KeyHash = lists:nth(rand:uniform(length(ExtraHash)), ExtraHash),
  ProcID = spawn(p2pchord, spawningnode, [KeyHash, LogN, NodesCreatedList, dict:new()]),
  [KeyHash, dict:store(KeyHash, ProcID, StateOfNetwork)].