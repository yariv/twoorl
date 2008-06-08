
%% Implements migration scripts
-module(twoorl_migrations).
-compile(export_all).
-include("twoorl.hrl").

m6() ->
    Msgs = msg:find(),
    {ok, Re} = regexp:parse("http://"),
    lists:foreach(
      fun(Msg) ->
	      BodyRaw = binary_to_list(msg:body_raw(Msg)),
	      ?L(BodyRaw),
	      {Body1, BodyNoLinks, Names} =
		  msg:process_raw_body(BodyRaw),
	      HasLinks = regexp:matches(BodyRaw, Re) =/= {match, []},
	      if HasLinks orelse Names =/= [] ->
		      msg:update([{body_nolinks, lists:flatten(BodyNoLinks)},
				  {body, lists:flatten(Body1)}], {id,'=',Msg:id()});
		 true ->
		      ok
	      end,
	      if HasLinks ->
		      receive
			  after 1000 -> ok
			  end;
		 true ->
		      ok
	      end
      end, Msgs).
