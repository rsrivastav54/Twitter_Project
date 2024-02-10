-module(user).
-behavior(gen_server).
-author("rishabhsrivastav").

%% API
-export([startClient/4, createDist/2, randomGen/1, manageLogin/2, handleLiveView/1, sndTwt/2, clientHandler/3, createListForSub/3, createZipfModel/2,
  manageRtwt/1, manageHashTag/2, manageQuery/1, manageMention/1, manageTwts/1, init/1]).

% start the client
startClient(UserId, NoOfTweets, NoToSubscribe, ExistingUser) ->
  gen_server:start_link(?MODULE, [UserId, NoOfTweets, NoToSubscribe, ExistingUser], []).

% enable distributed modelling
createDist([Head|Tail], L) ->
  NodeStatus = is_alive(),
  if NodeStatus == false ->
    try
      {IpTuple, _, _} = Head,
        CurrentIp = inet:ntoa(IpTuple),
        if CurrentIp =:= {127,0,0,1} ->
          if L > 1 ->
            createDist(Tail, L - 1);
            true -> io:format("Could not make current node distributed.~n")
          end;
          true ->
            ServerNodeName = list_to_atom(string:concat("client@", CurrentIp)),
            net_kernel:start(ServerNodeName),
            erlang:set_cookie(ServerNodeName, 'monster'),
            net_kernel:connect_node(string:concat("server@", CurrentIp))
        end
    catch
        _:_  ->
          if L > 1 ->
            createDist(Tail, L - 1);
            true -> io:format("Could not make current node distributed.~n")
          end
    end;
    true -> ok
  end.

% generate random text for tweet simulation
randomGen(L) ->
  Srb = crypto:strong_rand_bytes(L),
  Encode = base64:encode(Srb),
  Bp = binary_part(Encode, 0, L),
  string:lowercase(binary_to_list(Bp)).

% manage user login
manageLogin(UserId, 0) ->
  handleLiveView(UserId);
manageLogin(UserId, K) ->
  ServerPid = global:whereis_name(twitterServer),
  if K == 6 ->
    ServerPid ! {loginUser, UserId, self()},
    manageLogin(UserId, K - 1);
    true ->
      Tweet = "user" ++ UserId ++ " tweeting that " ++ randomGen(8) ++ " for testing",
      ServerPid ! {tweet, Tweet, UserId},
      manageLogin(UserId, K - 1)
  end.

% handle client view
handleLiveView(UserId) ->
  receive
    {live, Tweet} ->
      io:format("User ~p :- Live View -----~n", [UserId]),
      io:format(Tweet)
  end,
  handleLiveView(UserId).

% allow user to tweet
sndTwt(_, 0) -> ok;
sndTwt(UserId, NoOfTweets) ->
%%  io:format("text in send tweets = ~p~n", [randomizer(8)]),
  OutputString = "user" ++ UserId ++ " tweeting that " ++ randomGen(8) ++ " for testing",
  ServerPid = global:whereis_name(twitterServer),
  ServerPid ! {tweet, OutputString, UserId},
  sndTwt(UserId, NoOfTweets - 1).

% handle simulation tasks
clientHandler(UserId, NoOfTweets, NoToSubscribe) ->
  if NoToSubscribe > 0 ->
    SubList = createListForSub(1, NoToSubscribe, []),
    createZipfModel(UserId,SubList);
    true -> ok
  end,
  global:sync(),
  StartTime = erlang:system_time(millisecond),
  UserToMention = rand:uniform(list_to_integer(UserId)),
  shell:strings(true),
  OutputString = string:concat(string:concat("user", UserId), string:concat("tweeting @", integer_to_list(UserToMention))),
  ServerPid = global:whereis_name(twitterServer),
  ServerPid ! {tweet, OutputString, UserId},
  OutputString1 = string:concat(string:concat("user", UserId), " tweeting that #COP5615 is great"),
  ServerPid ! {tweet, OutputString1, UserId},
  sndTwt(UserId, NoOfTweets),
  TweetsTimeDiff = erlang:system_time(millisecond) - StartTime,
  StartTimeSubscribed = erlang:system_time(millisecond),
  manageQuery(UserId),
  QueriesSubscribedTimeDiff = erlang:system_time(millisecond) - StartTimeSubscribed,

  StartTimeHashTag = erlang:system_time(millisecond),
  manageHashTag("#COP5615", UserId),
  QueriesHashTagTimeDiff = erlang:system_time(millisecond) - StartTimeHashTag,

  StartTimeMentions = erlang:system_time(millisecond),
  manageMention(UserId),
  QueriesMentionTimeDiff = erlang:system_time(millisecond) - StartTimeMentions,

  StartTimeAllTweets = erlang:system_time(millisecond),
  manageTwts(UserId),
  QueriesMyTweetsTimeDiff = erlang:system_time(millisecond) - StartTimeAllTweets,

  NewTweetsTimeDiff = TweetsTimeDiff/(NoOfTweets + 3),
  MainPid = global:whereis_name(mainproc),
  MainPid ! {perfmetrics, NewTweetsTimeDiff, QueriesSubscribedTimeDiff, QueriesHashTagTimeDiff, QueriesMentionTimeDiff, QueriesMyTweetsTimeDiff},
  handleLiveView(UserId).

% create simulated list of subscribers
createListForSub(Count, NoOfSubs, List) ->
  if Count =:= NoOfSubs -> [Count | List];
    true -> createListForSub(Count + 1, NoOfSubs, [Count | List])
  end.

createZipfModel(UserId, SubscribeToList) ->
  global:sync(),
  ServerPid = global:whereis_name(twitterServer),
  lists:foreach(fun(AccountId) ->
    ServerPid ! {addSubscriber, UserId, integer_to_list(AccountId)} end, SubscribeToList).

manageRtwt(UserId) ->
  global:sync(),
  ServerPid = global:whereis_name(twitterServer),
  ServerPid ! {tweetsSubscribedTo, UserId},
  ListRec = receive
    {repTweetsSubscribedTo, List} -> List
  end,
  if ListRec =/= [] ->
    Rt = hd(ListRec),
    ServerPid ! {tweet, Rt ++ " -RT", UserId};
    true -> ok
  end.

manageQuery(UserId) ->
  global:sync(),
  ServerPid = global:whereis_name(twitterServer),
  ServerPid ! {tweetsSubscribedTo, UserId},
  receive
    {repTweetsSubscribedTo, List} ->
      if List =/= [] ->
        io:format("User ~p :- Subscription List: ~p~n", [UserId, List]);
        true -> ok
      end
  end.

manageHashTag(Tag, UserId) ->
  global:sync(),
  ServerPid = global:whereis_name(twitterServer),
  ServerPid ! {tweetsWithHashtag, UserId, Tag},
  receive
    {repTweetsWithHashtag, List} -> io:format("Tweets With Tag ~p: ~p~n", [Tag, List])
  end.

manageMention(UserId) ->
  global:sync(),
  Mention = string:concat("@", UserId),
  ServerPid = global:whereis_name(twitterServer),
  ServerPid ! {tweetsWithMention, Mention},
  receive
    {repTweetsWithMention, List} -> io:format("Tweets With Mention ~p: ~p~n", [Mention, List])
  end.

manageTwts(UserId) ->
  global:sync(),
  ServerPid = global:whereis_name(twitterServer),
  ServerPid ! {getMyTweets, UserId},
  receive
    {repGetMyTweets, List} ->
      if List =/= [] ->
        io:format("User ~p :- All my tweets : ~p~n", [UserId, List]);
        true -> ok
      end
  end.

initialization() ->
  {ok, Addrs} = inet:getifaddrs(),
  IpList = hd([
    Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
    size(Addr) == 4, Addr =/= {127,0,0,1}
  ]),
  Ip = inet:ntoa(IpList),
  ServerNodeName = list_to_atom(string:concat("client5@", Ip)),
  net_kernel:start([ServerNodeName]),
  erlang:set_cookie(node(), 'monster'),
  net_kernel:connect_node(list_to_atom(string:concat("server@", Ip))).

init([UserId, NoOfTweets, NoToSubscribe, ExistingUser]) ->
  initialization(),
  if ExistingUser =:= true ->
    io:format("User ~p :- reconnected~n", [UserId]),
    manageLogin(UserId, 6);
    true ->
      io:format("Welcome to Twitter!~n")
  end,
  global:sync(),
  ServerPid = global:whereis_name(twitterServer),
  ServerPid ! {registerUser, UserId, self()},
  receive
    {registerConfirmation} -> io:format("User ~p :- registered on server~n", [UserId])
  end,
  clientHandler(UserId, NoOfTweets, NoToSubscribe),
  receive
    {_} -> ok
  end.