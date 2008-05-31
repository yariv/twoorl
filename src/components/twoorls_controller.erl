-module(twoorls_controller).
-compile(export_all).

catch_all(_A, [_Username, Id]) ->
    %% currently, we ignore the username, but we keep it as a parameter
    %% for future user-based sharding
    case msg:find_id(list_to_integer(Id)) of
	undefined ->
	    exit({not_found, Id});
	Msg ->
	    {data, {twoorl_util:user_link(Msg:usr_username()),
		    Msg:body(),
		    Msg:get_time_since()}}
    end.
    
    
    
