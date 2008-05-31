-define(L(Msg), io:format("~p:~b ~p ~n", [?MODULE, ?LINE, Msg])).

-record(session, {key, value}).

-define(MIN_PASSWORD_LENGTH, 6).
-define(DEFAULT_KEY_SIZE, 20).
-define(MAX_PAGE_SIZE, 20).
-define(MAX_FOLLOWING_BOX_SIZE, 20).
-define(PAGING_WINDOW, 3).


%% -import(vimagi, [log/5]).
%% -define(Debug(Msg, Params),
%% 	vimagi:log(?MODULE, ?LINE, debug, fun() -> {Msg, Params} end)).
%% -define(Info(Msg, Params),
%% 	vimagi:log(?MODULE, ?LINE, info, fun() -> {Msg, Params} end)).
%% -define(Warn(Msg, Params),
%% 	vimagi:log(?MODULE, ?LINE, warn, fun() -> {Msg, Params} end)).
%% -define(Error(Msg, Params),
%% 	vimagi:log(?MODULE, ?LINE, error, fun() -> {Msg, Params} end)).

