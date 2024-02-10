-module(test).
-author("rishabhsrivastav").
-import(engine, [startServer/0]).
-import(user, [startClient/4]).
%% API
-export([delegate/1]).

% starting the simulation
delegate({}) ->
  spawn(fun() -> startServer() end),
  receive
    _ -> ok
  end;
delegate({NumClients, MaxSubscribers}) ->
  ets:new(mainregistry, [set, public, named_table]),
  ConvergencePid = spawn(fun() -> communicate(NumClients, NumClients, 0, 0, 0, 0, 0) end),
  global:sync(),
  global:register_name(mainproc, ConvergencePid),
  StartTime = erlang:system_time(millisecond),
  makeAccounts(1, NumClients, MaxSubscribers),
  receive
    _ -> ok
  end;
delegate({NumClients, MaxSubscribers, DisconnectClients}) ->
  ets:new(mainregistry, [set, public, named_table]),
  ConvergencePid = spawn(fun() -> timer:sleep(3000), communicate(NumClients, NumClients, 0, 0, 0, 0, 0) end),
  global:sync(),
  global:register_name(mainproc, ConvergencePid),
  StartTime = erlang:system_time(millisecond),
  makeAccounts(1, NumClients, MaxSubscribers),
%%  io:format("Time taken for inital simulation to complete: ~p milliseconds", [erlang:system_time(millisecond) - StartTime]),

  ClientsToDisconnect = DisconnectClients * (0.01) * NumClients,
  simulateDisconnection(NumClients, ClientsToDisconnect),
  receive
    _ -> ok
  end.

% communicating with client and server
communicate(0, TotalClients, TweetsTimeDiff, QueriesSubscribedTimeDiff,QueriesHashTagTimeDiff, QueriesMentionTimeDiff, QueriesMyTweetsTimeDiff) ->
  io:format("Average time to tweet: ~p milliseconds~n", [TweetsTimeDiff/TotalClients]),
  io:format("Average time to query tweets subscribe to: ~p~n milliseconds~n", [QueriesSubscribedTimeDiff/TotalClients]),
  io:format("Average time to query tweets by hashing: ~p~n milliseconds~n", [QueriesHashTagTimeDiff/TotalClients]),
  io:format("Average time to query tweets by mention: ~p~n milliseconds~n", [QueriesMentionTimeDiff/TotalClients]),
  io:format("Average time to query all relevant tweets: ~p~n milliseconds~n", [QueriesMyTweetsTimeDiff/TotalClients]);
communicate(NumClients, TotalClients, TweetsTimeDiff, QueriesSubscribedTimeDiff,QueriesHashTagTimeDiff, QueriesMentionTimeDiff, QueriesMyTweetsTimeDiff) ->
  receive
    {perfmetrics,A,B,C,D,E} -> communicate(NumClients - 1, TotalClients, TweetsTimeDiff + A, QueriesSubscribedTimeDiff + B,
      QueriesHashTagTimeDiff + C, QueriesMentionTimeDiff + D, QueriesMyTweetsTimeDiff + E)
  end.

% creating user simulated accounts
makeAccounts(Count, NoOfClients, TotalSubscribers) ->
  UserName = integer_to_list(Count),
  NoOfTweets = round(floor(TotalSubscribers / Count)),
  NoToSubscribe = round(floor(TotalSubscribers / (NoOfClients - Count + 1))) - 1,
  Pid = spawn(fun() -> startClient(UserName, NoOfTweets, NoToSubscribe, false) end),
  ets:insert(mainregistry, {UserName, Pid}),
  if Count =/= NoOfClients ->
    makeAccounts(Count + 1, NoOfClients, TotalSubscribers);
    true -> ok
  end.

simulateDisconnection(NumClients, ClientsToDisconnect) ->
  timer:sleep(1000),
  DisconnectList = handleDisconnection(NumClients, ClientsToDisconnect, 0, []),
  timer:sleep(1000),
  lists:foreach(fun(UserName) -> Pid = spawn(fun() -> startClient(UserName, -1,-1, true) end),
    ets:insert(mainregistry, {UserName, Pid}) end, DisconnectList),
  simulateDisconnection(NumClients, ClientsToDisconnect).

handleDisconnection(NumClients, ClientsToDisconnect, ClientsDisconnected, DisconnectList) ->
  if ClientsDisconnected < ClientsToDisconnect ->
    DisconnectClient = rand:uniform(NumClients),
    DisconnectClientId = whereIs(integer_to_list(DisconnectClient)),
    if DisconnectClientId =/= nil ->
      UserId = integer_to_list(DisconnectClient),
      NewDisconnectList = [UserId | DisconnectList],
      global:sync(),
      ServerPid = global:whereis_name(twitterServer),
      ServerPid ! {disconnectUser, UserId},
      ets:insert(mainregistry, {UserId, nil}),
      exit(DisconnectClientId, kill),
      io:format("Simulator :- User ~p has been disconnected~n", [UserId]),
      handleDisconnection(NumClients, ClientsToDisconnect, ClientsDisconnected + 1, NewDisconnectList);
      true -> handleDisconnection(NumClients, ClientsToDisconnect, ClientsDisconnected, DisconnectList)
    end;
    true -> DisconnectList
  end.

whereIs(UserId) ->
  Val = ets:lookup(mainregistry, UserId),
  if Val == [] -> nil;
    true ->
      [{_,Tup}] = ets:lookup(mainregistry, UserId),
      Tup
  end.
