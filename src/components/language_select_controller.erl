-module(language_select_controller).
-compile(export_all).
-include("twoorl.hrl").

private() ->
    true.

index(A) ->
    Lang = case erlyweb_util:get_cookie("lang", A) of
	       undefined -> <<"eng">>;
	       Other -> list_to_binary(Other)
	   end,
    {data, Lang}.
