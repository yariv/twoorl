-define(L(Msg), io:format("~p:~b ~p ~n", [?MODULE, ?LINE, Msg])).

-record(session, {key, value}).

-define(MIN_PASSWORD_LENGTH, 6).
-define(DEFAULT_KEY_SIZE, 20).
-define(MAX_PAGE_SIZE, 20).
-define(MAX_FOLLOWING_BOX_SIZE, 20).
-define(PAGING_WINDOW, 3).
-define(MAX_TWOORL_LEN, 140).
-define(MAX_MSG_SIZE, 10000).

-define(TWITTER_NOT_SENT, 0).
-define(TWITTER_SENT_PENDING, 1).
-define(TWITTER_SENT_OK, 2).
-define(TWITTER_SENT_ERR, 3).
-define(DEFAULT_GRAVATAR_ID, <<"98dbdc9e0d214030d3ee1c77c502248f">>).

-define(Debug(Msg, Params),
	twoorl_util:log(?MODULE, ?LINE, debug, fun() -> {Msg, Params} end)).
-define(Info(Msg, Params),
 	twoorl_util:log(?MODULE, ?LINE, info, fun() -> {Msg, Params} end)).
-define(Warn(Msg, Params),
 	twoorl_util:log(?MODULE, ?LINE, warn, fun() -> {Msg, Params} end)).
-define(Error(Msg, Params),
 	twoorl_util:log(?MODULE, ?LINE, error, fun() -> {Msg, Params} end)).

