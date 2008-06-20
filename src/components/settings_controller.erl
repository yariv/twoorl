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
	    Background = proplists:get_value("background", Params),
	    
	    
	    {[TwitterUsername, TwitterPassword], Errs} =
		erlyweb_forms:validate(
		  Params,
		  ["twitter_username", "twitter_password"],
		  ValidationFun),
	    Errs1 = verify_twitter_credentials(
		      TwitterEnabled, TwitterUsername, TwitterPassword),
	    Errs2 = case Background of
			undefined -> Errs1;
			"http://" ++ _ -> Errs1;
			_ -> [{invalid_url, <<"background image">>} | Errs1]
		    end,
	    Errs3 = Errs ++ Errs2,
	    case Errs3 of
		[] ->
		    Usr2 =
			update_settings(
			  Usr, TwitterUsername, TwitterPassword,
			  TwitterEnabled, GravatarEnabled, Background),
		    twoorl_util:update_session(A,Usr2),
		    {ewr, settings, ["?success=true"]};
		_ ->
		    [result_data(
		       A,
		       TwitterUsername, TwitterPassword,
		       checked(TwitterEnabled), checked(GravatarEnabled),
		       str(Background)),
		     {ewc, ui_msgs, [A, Errs3, []]}]
	    end;
	_ ->
	    UiMessages = case lists:member({"success", "true"},
					yaws_api:parse_query(A)) of
			     true ->
			       [settings_updated];
			     false ->
				 []
			 end,
	    [result_data(A, str(usr:twitter_username(Usr)),
			 str(usr:twitter_password(Usr)),
			 checked(usr:twitter_enabled(Usr)),
			 checked(usr:gravatar_enabled(Usr)),
			 str(usr:background(Usr))),
	     {ewc, ui_msgs, [A, [], UiMessages]}]
    end.

result_data(A, TwitterUsername, TwitterPassword, TwitterEnabled,
	    GravatarEnabled, Background) ->
    ?Data(A, {TwitterUsername, TwitterPassword, TwitterEnabled,
	      GravatarEnabled, Background}).


get_validation_fun(true) ->
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
get_validation_fun(_) ->
    fun(_Field, _Val) ->
	    ok
    end.

verify_twitter_credentials(_, [], _) -> [];
verify_twitter_credentials(_, _, []) -> [];
verify_twitter_credentials(true, Username, Password) ->
    case twitter_client:account_verify_credentials(Username, Password, []) of
        true -> [];
        false -> [twitter_unauthorized]
    end;
verify_twitter_credentials(_, _, _) -> [].

update_settings(Usr, TwitterUsername, TwitterPassword, TwitterEnabled,
	  GravatarEnabled, Background) ->
    Usr1 = usr:set_fields(
	    Usr,
	    [{twitter_username, TwitterUsername},
	     {twitter_password, TwitterPassword},
	     {twitter_enabled, bool_to_int(TwitterEnabled)},
	     {gravatar_enabled, bool_to_int(GravatarEnabled)},
	     {background, Background}]),
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
str(Val) when is_list(Val) -> list_to_binary(Val);
str(Val) -> Val.

checked(0) -> [];
checked(false) -> [];
checked(1) -> <<"checked">>;
checked(true) -> <<"checked">>;
checked(undefined) -> [].    
    
is_checked(Param, Params) ->
    proplists:get_value(Param, Params)  == "on".

bool_to_int(false) -> 0;
bool_to_int(true) -> 1.
