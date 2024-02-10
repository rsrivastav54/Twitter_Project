-module(server).
-author("rishabhsrivastav").
-import(string,[substr/3]).
-compile(export_all).

% refer to engine.erl to understand what each snippet does as functions are similar
start()->
    io:fwrite("Twitter Engine Start\n"),
    ets:new(user_registry, [set, public, named_table]),
    ets:new(tweet, [set, public, named_table]),
    ets:new(subscriptions, [set, public, named_table]),
    ets:new(followers, [set, public, named_table]),
    ets:new(hashTags, [set, public, named_table]),
    ets:new(mentions, [set, public, named_table]).

register_user(User_ID)->
    case check_user_exist(User_ID) of
        {0} ->
            io:fwrite("Creating account with ID: ~p\n", [User_ID]),
            ets:insert(user_registry,{User_ID}),
            ets:insert(tweet, {User_ID, []}),
            ets:insert(subscriptions , {User_ID, []}),
            ets:insert(followers , {User_ID, []}),
            ets:insert(hashTags, {User_ID, []}),
            ets:insert(mentions, {User_ID, []}),
            io:fwrite("Account created successfully... \n");
        {1} ->
            io:fwrite("Account already exists, proceed to tweeting!\n")
    end.

send_tweet(User_ID, Tweet_content)->
    case check_user_exist(User_ID) of
        {1} ->
            io:fwrite("User: ~p is tweeting: ~p\n", [User_ID, Tweet_content]),
            process_hashtags(Tweet_content),
            process_mentions(Tweet_content),
            List = ets:lookup(tweet, User_ID),
            [{_,Tweets}] = List,
            L = lists:append(Tweets,[Tweet_content]),
            ets:insert(tweet, {User_ID, L}),
            Followers = get_my_followers(User_ID),
            send_tweet_to_followers(Followers, Tweet_content);
        {0} ->
            io:fwrite("Kindly register first before tweeting\n")
    end.

send_tweet_to_followers([], _) ->
    ok;
send_tweet_to_followers([H|T], Tweet) ->
    List = ets:lookup(tweet, H),
    [{_,Tweets}] = List,
    io:fwrite("~p ~p\n",[H,Tweets]),
    L = lists:append(Tweets,[Tweet]),
    ets:insert(tweet, {H, L}),
    send_tweet_to_followers(T, Tweet).

process_hashtags(Tweet) ->
    Regex = "#[a-zA-Z0-9]+",
    case re:run(Tweet, Regex, [global]) of
        {match, Captured} ->  
            add_tag(Captured, Tweet);
        nomatch ->
            ok
    end.

process_mentions(Tweet) ->
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
    % io:fwrite("Tag:~p Tweet:~p\n",[Tag, Tweet]),
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

get_my_tweets(User_ID)->
    io:fwrite("All tweets by User: ~p , tweets list: \n", [User_ID]),
    List = ets:lookup(tweet, User_ID),
    [{_,Tweets}] = List,
    Tweets.

get_tweets_list(User_ID)->
    List = ets:lookup(tweet, User_ID),
    [{_,Tweets}] = List,
    Tweets.

get_tweets_with_tag(Tag) ->
    case ets:lookup(hashTags, Tag) of
        [{Tag,_}] ->
            List = ets:lookup(hashTags, Tag),
            [{_,Tweets}] = List,
            Tweets;
        [] ->
            io:fwrite("No tweets exist with #~p\n",[Tag])
    end.

get_tweets_with_mention(Tag) ->
    case ets:lookup(mentions, Tag) of
        [{Tag,_}] ->
            List = ets:lookup(mentions, Tag),
            [{_,Tweets}] = List,
            Tweets;
        [] ->
            io:fwrite("No tweets exist with mention @~p\n",[Tag])
    end.

subscribe(User_ID, Sub_ID)->
    case check_if_subscribed(User_ID,Sub_ID) of
        {0} ->
            io:fwrite("User: ~p has subscribed to user: ~p!!\n", [User_ID, Sub_ID]),
            User_Tweets = get_tweets_list(User_ID),
            Sub_Tweets = get_tweets_list(Sub_ID),
            User_new_Tweets = lists:append(User_Tweets,Sub_Tweets),
            ets:insert(tweet, {User_ID, User_new_Tweets}),
            User = ets:lookup(subscriptions, User_ID),
            [{_,Subscription}] = User,
            Subscription_List = lists:append(Subscription,[Sub_ID]),
            ets:insert(subscriptions,{User_ID, Subscription_List}),
            add_follower(Sub_ID, User_ID);
        {1} ->
            io:fwrite("User has already subscribed.\n")
    end.

add_follower(Sub_ID, User_ID) ->
    io:fwrite("Adding user: ~p to user: ~p follower list \n", [User_ID, Sub_ID]),
    User = ets:lookup(followers, Sub_ID),
    [{_,Followers}] = User,
    Followers_List = lists:append(Followers,[User_ID]),
    ets:insert(followers,{Sub_ID, Followers_List}).

get_my_subscriptions(User_ID)->
    io:fwrite("User: ~p subscription list \n", [User_ID]),
    List = ets:lookup(subscriptions, User_ID),
    [{_,Subscriptions}] = List,
    Subscriptions.

get_my_followers(User_ID)->
    io:fwrite("User: ~p follower list \n", [User_ID]),
    List = ets:lookup(followers, User_ID),
    [{_,Followers}] = List,
    Followers.

check_user_exist(User_ID)->
    case ets:lookup(user_registry, User_ID) of
        [{User_ID}] ->
            {1};
        [] ->
            {0}
    end.

check_if_subscribed(User_ID, Sub_ID)->
    List = ets:lookup(subscriptions, User_ID),
    [{_,Subscriptions}] = List,
    case lists:member(Sub_ID, Subscriptions) of
        true ->
            {1};
        false ->
            {0}
    end.

