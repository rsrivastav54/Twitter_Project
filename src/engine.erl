-module(engine).
-author("rishabhsrivastav").
-behavior(gen_server).
-import(string,[substr/3]).
-export([init/1, startServer/0]).
-compile(export_all).

% starting the twitter engine
startServer() ->
    gen_server:start_link(?MODULE, [], []).

% function to find user in database
find(UserId) ->
    Val = ets:lookup(user_registry, UserId),
    if Val == [] -> nil;
        true ->
            [{_,Tup}] = ets:lookup(user_registry, UserId),
            Tup
    end.

% enabling distributed modelling
modelDist([Head|Tail], L) ->
    NodeStatus = is_alive(),
    if NodeStatus == false ->
        try
            {IpTuple, _, _} = Head,
            CurrentIp = inet:ntoa(IpTuple),
            if CurrentIp =:= {127,0,0,1} ->
                if L > 1 ->
                    modelDist(Tail, L - 1);
                    true -> io:format("Could not make current node distributed.~n")
                end;
                true ->
                    io:format(CurrentIp),
                    ServerNodeName = list_to_atom(string:concat("server@", CurrentIp)),
                    net_kernel:start(ServerNodeName),
                    erlang:set_cookie(ServerNodeName, 'monster')
            end
        catch
            _:_  ->
                if L > 1 ->
                    modelDist(Tail, L - 1);
                    true -> io:format("Could not make current node distributed.~n")
                end
        end;
        true -> ok
    end.

% initializing server IP
initialization(Ip) ->
    ServerName = list_to_atom(string:concat("server@", Ip)),
    net_kernel:start([ServerName]),
    erlang:set_cookie(node(), 'monster').

init(_Args)->
    {ok, [Ip]} = io:fread("Enter your Ip:", "~s"),
    initialization(Ip),
    io:fwrite("Twitter Engine Start\n"),
    ets:new(user_registry, [set, public, named_table]),
    ets:new(tweet, [set, public, named_table]),
    ets:new(subscriptions, [set, public, named_table]),
    ets:new(followers, [set, public, named_table]),
    ets:new(hashTags, [set, public, named_table]),
    ets:new(mentions, [set, public, named_table]),
    ServerPid = spawn_link(fun() -> apiHandler() end),
    global:register_name(twitterServer, ServerPid),
    io:format("Server Started~n"),
    receive
        _ -> ok
    end.

apiHandler() ->
    receive
        {registerUser, UserId, Pid} ->
            reg_account(UserId, Pid),
            Pid ! {registerConfirmation};
        {tweet, Tweet, UserId} -> create_twt(UserId, Tweet);
        {tweetsSubscribedTo, UserId} -> spawn(fun() -> return_subs(UserId) end);
        {tweetsWithHashtag, UserId, HashTag} -> spawn(fun() -> query_twt_with_tag(UserId, HashTag) end);
        {tweetsWithMention, Mention} -> spawn(fun() -> return_twt_men(Mention) end);
        {getMyTweets, UserId} -> spawn(fun() -> return_all_twts(UserId) end);
        {addSubscriber, UserId, SubId} -> sub(UserId, SubId)
    end,
    apiHandler().

% register user account on server
reg_account(User_ID, Pid)->
    case check_user_exist(User_ID) of
        {0} ->
            io:fwrite("Creating account with ID: ~p\n", [User_ID]),
            ets:insert(user_registry,{User_ID, Pid}),
            ets:insert(tweet, {User_ID, []}),
            ets:insert(subscriptions , {User_ID, []}),
            ets:insert(followers , {User_ID, []}),
            ets:insert(hashTags, {User_ID, []}),
            ets:insert(mentions, {User_ID, []}),
            io:fwrite("Account created successfully... \n");
        {1} ->
            io:fwrite("Account already exists, proceed to tweeting!\n")
    end.

% allow user to generate tweets
create_twt(User_ID, Tweet_content)->
    shell:strings(true),
    case check_user_exist(User_ID) of
        {1} ->
            io:fwrite("User: ~p is tweeting: ~p\n", [User_ID, Tweet_content]),
            query_htag(Tweet_content),
            query_mention(Tweet_content),
            List = ets:lookup(tweet, User_ID),
            [{_,Tweets}] = List,
            L = lists:append(Tweets,[Tweet_content]),
            ets:insert(tweet, {User_ID, L}),
            Followers = return_flwrs(User_ID, Tweet_content),
            publish_twt(Followers, Tweet_content);
        {0} ->
            io:fwrite("To tweet you must first register.\n")
    end.

% allow user to send tweets
publish_twt([], _) ->
    ok;
publish_twt([H|T], Tweet) ->
    List = ets:lookup(tweet, H),
    [{_,Tweets}] = List,
    io:fwrite("~p ~p\n",[H,Tweets]),
    L = lists:append(Tweets,[Tweet]),
    ets:insert(tweet, {H, L}),
    publish_twt(T, Tweet).

% Querying to find hashtags
query_htag(Tweet) ->
    Regex = "#[a-zA-Z0-9]+",
    case re:run(Tweet, Regex, [global]) of
        {match, Captured} ->  
            add_tag(Captured, Tweet);
        nomatch ->
            ok
    end.

% Querying to find mentions
query_mention(Tweet) ->
    Regex = "@[a-zA-Z0-9]+",
    case re:run(Tweet, Regex, [global]) of
        {match, Captured} ->  
            add_mention(Captured, Tweet);
        nomatch ->
            ok
    end.

add_tag([], _) ->
    ok;
add_tag([H|T], Tweet) ->
    [{Start,Length}] = H,
    Tag = substr(Tweet,Start+1,Length),
%%    io:fwrite("Tag:~p \t Tweet:~p\n",[Tag, Tweet]),
    case ets:lookup(hashTags, Tag) of
        [] ->
            ets:insert(hashTags, {Tag, [Tweet]}),
            add_tag(T, Tweet);
        [{Tag,_}] ->
            List = ets:lookup(hashTags, Tag),
            [{_,Tweets}] = List,
            L = lists:append(Tweets,[Tweet]),
            ets:insert(hashTags, {Tag, L}),
            add_tag(T, Tweet)
    end.

add_mention([], _) ->
    ok;
add_mention([H|T], Tweet) ->
    [{Start,Length}] = H,
    Tag = substr(Tweet,Start+1,Length),
%%    io:fwrite("Mention:~p \t Tweet:~p\n",[Tag, Tweet]),
    case ets:lookup(mentions, Tag) of
        [] ->
            ets:insert(mentions, {Tag, [Tweet]}),
            add_tag(T, Tweet);
        [{Tag,_}] ->
            List = ets:lookup(mentions, Tag),
            [{_,Tweets}] = List,
            L = lists:append(Tweets,[Tweet]),
            ets:insert(mentions, {Tag, L}),
            add_tag(T, Tweet)
    end.

return_all_twts(User_ID)->
    List = ets:lookup(tweet, User_ID),
    [{_,Tweets}] = List,
    io:fwrite("All tweets by User: ~p , tweets list: ~p \n", [User_ID, Tweets]),
    Pid = find(User_ID),
    Pid ! {repGetMyTweets, Tweets}.

return_twt_list(User_ID)->
    List = ets:lookup(tweet, User_ID),
    io:format("In get tweets - ~p~n", [List]),
    if List =/= [] -> [{_,Tweets}] = List;
        true -> Tweets = []
    end,
    Tweets.

query_twt_with_tag(UserId, Tag) ->
    case ets:lookup(hashTags, Tag) of
        [{Tag,_}] ->
            List = ets:lookup(hashTags, Tag),
            [{_,Tweets}] = List,
            io:format("Tweets with HashTag - ~p : ~p~n", [Tag, Tweets]),
%%            Tweets,
            Pid = find(UserId),
            Pid ! {repTweetsWithHashtag, Tweets};
        [] ->
            io:fwrite("No tweets exist with #~p\n",[Tag])
    end.

return_twt_men(Tag) ->
    case ets:lookup(mentions, Tag) of
        [{Tag,_}] ->
            List = ets:lookup(mentions, Tag),
            [{_,Tweets}] = List,
            io:format("Tweets with Mention: ~p : ~p~n", [Tag, Tweets]),
%%            Tweets,
            [_, UserId] = string:split(Tag, "@"),
            Pid = find(UserId),
            Pid ! {repTweetsWithMention, Tweets};
        [] ->
            io:fwrite("No tweets exist with mention @~p\n",[Tag])
    end.

sub(User_ID, Sub_ID)->
    case check_if_subscribed(User_ID,Sub_ID) of
        {0} ->
            io:fwrite("User: ~p has subscribed to user: ~p!\n", [User_ID, Sub_ID]),
            User_Tweets = return_twt_list(User_ID),
            Sub_Tweets = return_twt_list(Sub_ID),
            User_new_Tweets = lists:append(User_Tweets,Sub_Tweets),
            ets:insert(tweet, {User_ID, User_new_Tweets}),
            User = ets:lookup(subscriptions, User_ID),
            [{_,Subscription}] = User,
            Subscription_List = lists:append(Subscription,[Sub_ID]),
            ets:insert(subscriptions,{User_ID, Subscription_List}),
            create_flwr(Sub_ID, User_ID);
        {1} ->
            io:fwrite("User has already subscribed.\n")
    end.

% adding followers to user
create_flwr(Sub_ID, User_ID) ->
    io:fwrite("Adding user: ~p to user: ~p follower list.\n", [User_ID, Sub_ID]),
    User = ets:lookup(followers, Sub_ID),
    [{_,Followers}] = User,
    Followers_List = lists:append(Followers,[User_ID]),
    ets:insert(followers,{Sub_ID, Followers_List}).

% returning subscriber list
return_subs(User_ID)->
%%    io:fwrite("User:  ~p subscription list:!!\n", [User_ID]),
    List = ets:lookup(subscriptions, User_ID),
    [{_,Subscriptions}] = List,
    io:fwrite("User: ~p subscription list:~p\n", [User_ID, Subscriptions]),
    Subscriptions,
    Pid = find(User_ID),
    Pid ! {repTweetsSubscribedTo, Subscriptions}.

% returning followers list
return_flwrs(User_ID, TweetContent)->
    io:fwrite("User: ~p follower list:!!\n", [User_ID]),
    List = ets:lookup(followers, User_ID),
    [{_,Followers}] = List,
    lists:foreach(fun(Follower) ->
        FollowerPid = find(Follower),
        if FollowerPid =/= nil ->
            FollowerPid ! {live, TweetContent};
            true -> ok
        end
                  end, Followers
    ),
    Followers.

% check if user is dead or alive
check_user_exist(User_ID)->
    case ets:lookup(user_registry, User_ID) of
        [{User_ID, _}] ->
            {1};
        [] ->
            {0}
    end.

% check for subscription
check_if_subscribed(User_ID, Sub_ID)->
    List = ets:lookup(subscriptions, User_ID),
    [{_,Subscriptions}] = List,
    case lists:member(Sub_ID, Subscriptions) of
        true ->
            {1};
        false ->
            {0}
    end.

disconnectUser(UserId) -> ets:insert(user_registry, {UserId, nil}).