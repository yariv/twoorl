
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
	      {Body1, BodyNoLinks, _Names} =
		  msg:process_raw_body(BodyRaw),
	      HasLinks = regexp:matches(BodyRaw, Re) =/= {match, []},
	      msg:update([{body_nolinks, lists:flatten(BodyNoLinks)},
			  {body, lists:flatten(Body1)}], {id,'=',Msg:id()}),
	      if HasLinks ->
		      receive
			  after 1000 -> ok
			  end;
		 true ->
		      ok
	      end
      end, Msgs).

%% change user urls from /users/[Username] to /[Username]
m8() ->
    Msgs = msg:find(),
    {ok, Re} = regexp:parse("/users/"),
    lists:foreach(
      fun(Msg) ->
	      Body = binary_to_list(Msg:body()),
	      {ok, Body1, _N} = regexp:gsub(Body, Re, "/"),
	      
	      msg:update([{body, Body1}], {id,'=',Msg:id()})
      end, Msgs).
    

m9() ->
    Users = usr:find({spammer,'=',1}),
    lists:foreach(
      fun(Usr) ->
	      msg:update([{spam, 1}], {usr_id,'=',Usr:id()})
      end, Users).
	      
    
