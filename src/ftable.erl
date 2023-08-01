%%%-------------------------------------------------------------------
%%% @author Sripriya Simhadri - Neha Komuravelly
%%% @copyright (C) 2022, DOSP COP5615
%%% @doc
%%%
%%% @end
%%% Created : 20. Oct 2022 09:21 AM
%%%-------------------------------------------------------------------

-module(ftable).
-author("simkom").
-export([circulate_fingertbl/2]).

fetch_fingertbl(_, _, M, M,FingerList) ->
  FingerList;
fetch_fingertbl(Node, StateOfNetwork, M, I, FingerList) ->
  KeyHash = element(1, Node),
  Ith_succesor = find_nth_succ(KeyHash, StateOfNetwork, trunc(math:pow(2, I)), 0, KeyHash, M),
  fetch_fingertbl(Node, StateOfNetwork, M, I + 1, FingerList ++ [{Ith_succesor, dict:fetch(Ith_succesor, StateOfNetwork)}] ).

circulate_fingertbl(StateOfNetwork,M) ->
  FngrTbls = clctFngrTbls(StateOfNetwork, dict:to_list(StateOfNetwork), dict:new(),M),
  circulate_fingertbl_nodes(dict:fetch_keys(FngrTbls), StateOfNetwork, FngrTbls).

circulate_fingertbl_nodes([], _, _) ->
  ok;
circulate_fingertbl_nodes(SendN, StateOfNetwork, FngrTbls) ->
  [One|Extra] = SendN,
  ProcID = dict:fetch(One ,StateOfNetwork),
  ProcID ! {fingertbl, dict:from_list(dict:fetch(One, FngrTbls))},
  circulate_fingertbl_nodes(Extra, StateOfNetwork, FngrTbls).

clctFngrTbls(_, [], FngrTblD,_) ->
  FngrTblD;

clctFngrTbls(StateOfNetwork, NtwkL, FngrTblD,LogN) ->
  [One | Extra] = NtwkL,
  FngrTbls = fetch_fingertbl(One, StateOfNetwork,LogN, 0,[]),
  clctFngrTbls(StateOfNetwork, Extra, dict:store(element(1, One), FngrTbls, FngrTblD), LogN).


find_nth_succ(_, _, I , I, PointerID, M) ->
  PointerID;

find_nth_succ(KeyHash, StateOfNetwork, I, Ptr, PointerID, M) ->
  case dict:find((PointerID + 1) rem trunc(math:pow(2, M)), StateOfNetwork) of
    error ->
      find_nth_succ(KeyHash, StateOfNetwork, I, Ptr, (PointerID + 1) rem trunc(math:pow(2, M)),M);
    _ -> find_nth_succ(KeyHash, StateOfNetwork, I, Ptr + 1, (PointerID + 1) rem trunc(math:pow(2, M)),M)
  end.