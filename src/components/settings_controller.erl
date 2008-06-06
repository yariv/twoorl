-module(settings_controller).
-export([index/1]).
-include("twoorl.hrl").

index(A) ->
    twoorl_util:auth(A, fun(Usr) -> process_request(A, Usr) end).


process_request(A, Usr) ->

    case yaws_arg:method(A) of
	'POST' ->
	    Params = yaws_api:parse_post(A),
	    TwitterEnabled = is_checked("twitter_enabled", Params),
	    GravatarEnabled = is_checked("gravatar_enabled", Params),
	    ValidationFun = get_validation_fun(TwitterEnabled),
	    
	    {[TwitterUsername, TwitterPassword], Errs} =
		erlyweb_forms:validate(
		  Params,
		  ["twitter_username", "twitter_password"],
		  ValidationFun),
	    Errs1 = verify_twitter_credentials(
		      TwitterEnabled, TwitterUsername, TwitterPassword),
	    Errs2 = Errs ++ Errs1,
	    Messages =
		case Errs2 of
		    [] ->
			Usr2 =
			    update_settings(
			      Usr, TwitterUsername, TwitterPassword,
			      TwitterEnabled, GravatarEnabled),
			twoorl_util:update_session(A,Usr2),
			[{updated, <<"your settings">>}];

		    _ ->
			[]
		end,
	    {data, {TwitterUsername, TwitterPassword,
		    checked(TwitterEnabled), checked(GravatarEnabled), Errs2,
		    Messages}};
	_ ->
	    {data, {str(usr:twitter_username(Usr)),
		    str(usr:twitter_password(Usr)),
		    checked(usr:twitter_enabled(Usr)),
		    checked(usr:gravatar_enabled(Usr)),
		    [],
		    []}}
    end.

get_validation_fun(TwitterEnabled) ->
    if TwitterEnabled ->
	    fun(Field, Val) ->
		    case Val of
			[] ->
			    FName = case Field of
					"twitter_username" ->
					    "Twitter username";
					"twitter_password" ->
					    "Twitter password"
				    end,
			    {error, {missing_field, FName}};
			_ ->
			    ok
		    end
	    end;
       true ->
	    fun(_Field, _Val) ->
		    ok
	    end
    end.

verify_twitter_credentials(TwitterEnabled, TwitterUsername, TwitterPassword) ->
    if TwitterEnabled andalso not TwitterUsername == []
       andalso not TwitterPassword == [] ->
	    case twitter:verify_credentials(
		   TwitterUsername, TwitterPassword) of
		ok ->
		    [];
		{error, unauthorized} ->
		    [twitter_unauthorized];
		{error, Err} ->
		    ?Error("twitter authorization error: ~p ~p ~p",
			   [TwitterUsername, TwitterPassword,
			    Err]),
		    [twitter_authorization_error]
	    end;
       true ->
	    []
    end.

update_settings(Usr, TwitterUsername, TwitterPassword, TwitterEnabled,
	  GravatarEnabled) ->
    Usr1 = usr:set_fields(
	    Usr,
	    [{twitter_username, TwitterUsername},
	     {twitter_password, TwitterPassword},
	     {twitter_enabled, bool_to_int(TwitterEnabled)},
	     {gravatar_enabled, bool_to_int(GravatarEnabled)}]),
    Usr2 = Usr1:save(),
    LastGravatarStatus = Usr:gravatar_enabled(),
    if GravatarEnabled == LastGravatarStatus ->
	    ok;
       true ->
	    %% TODO consider doing this in a
	    %% background processes
	    msg:update([{usr_gravatar_enabled,
			 bool_to_int(GravatarEnabled)}],
		       {usr_id,'=',Usr:id()})
    end,
    Usr2.


str(undefined) -> [];
str(Val) -> Val.

checked(0) -> [];
checked(false) -> [];
checked(1) -> <<"checked">>;
checked(true) -> <<"checked">>.
    
    
is_checked(Param, Params) ->
    proplists:get_value(Param, Params)  == "on".

bool_to_int(false) -> 0;
bool_to_int(true) -> 1.
