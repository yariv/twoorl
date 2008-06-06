%% This file is part of Twoorl.
%% 
%% Twoorl is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% Twoorl is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with Twoorl.  If not, see <http://www.gnu.org/licenses/>.
%%
%% Copyright Yariv Sadan, 2008
%%
%% @author Yariv Sadan <yarivsblog@gmail.com> [http://yarivsblog.com]
%% @copyright Yariv Sadan, 2008

-module(twoorl_util).
-compile(export_all).
-include("twoorl.hrl").

gen_key() ->
    gen_key(?DEFAULT_KEY_SIZE).

gen_key(Len) ->
    Chars =
	{$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,
	 $w,$x,$y,$z,
	 $A,$B,$C,$D,$E,$F,$G,$H,$I,$J,$K,$L,$M,$N,$O,$P,$Q,$R,$S,$T,$U,$V,
	 $W,$X,$Y,$Z,
	 $1,$2,$3,$4,$5,$6,$7,$8,$9,$0},
    Res = lists:map(
	    fun(_) ->
		    Idx = crypto:rand_uniform(1, 62),
		    element(Idx, Chars)
	    end, lists:seq(1, Len)),
    list_to_binary(Res).


join(List) ->
    join(List, ",").

join(List, Delim) ->
    lists:foldl(
      fun(Elem, []) ->
	      [Elem];
	 (Elem, Acc) ->
	      [Elem, Delim | Acc]
      end, [], lists:reverse(List)).
	      
    
get_seconds_since({datetime, Val}) ->
    get_seconds_since(Val);
get_seconds_since(Val) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:local_time()) -
	calendar:datetime_to_gregorian_seconds(Val).

get_time_since(DateTime) ->
    Diff = 
	get_seconds_since(DateTime),
    {Val1, UnitStr} =
	if Diff < 60 ->
		{Diff, <<"seconds">>};
	   Diff < 3600 ->
		{round(Diff / 60), <<"minutes">>};
	   Diff < 86400 ->
		{round(Diff / 3600), <<"hours">>};
	   true ->
		{round(Diff / 86400), <<"days">>}
	end,
    [integer_to_list(Val1), 32, UnitStr, <<" ago">>].


auth(A, Fun) ->
    auth(A, Fun, fun() -> {ewr, login} end).

auth(A, Fun, ElseFun) ->
    case get_usr(A) of
	undefined ->
	    ElseFun();
	Usr ->
	    Fun(Usr)
    end.

get_usr(A) ->
    Session = yaws_arg:get_opaque_val(A, session),
    if Session =/= undefined ->
	    Session#session.value;
       true ->
	    undefined
    end.


htmlize(undefined) -> undefined;
htmlize(List) when is_list(List) -> htmlize_l(List, []). % modified by Michael

htmlize_l(List) -> htmlize_l(List, []).
htmlize_l([], Acc) -> lists:reverse(Acc);

htmlize_l([13,10|Tail], Acc) -> htmlize_l(Tail, ["<br/>"|Acc]);
htmlize_l([13|Tail], Acc) -> htmlize_l(Tail, ["<br/>"|Acc]);
htmlize_l([10|Tail], Acc) -> htmlize_l(Tail, ["<br/>"|Acc]);
htmlize_l([$>|Tail], Acc) -> htmlize_l(Tail, [$;,$t,$g,$&|Acc]);
htmlize_l([$<|Tail], Acc) -> htmlize_l(Tail, [$;,$t,$l,$&|Acc]);
htmlize_l([$&,A,$;|Tail], Acc) -> htmlize_l(Tail, [$;,A,$&|Acc]);
htmlize_l([$&,A,B,$;|Tail], Acc) -> htmlize_l(Tail, [$;,B,A,$&|Acc]);
htmlize_l([$&,A,B,C,$;|Tail], Acc) -> htmlize_l(Tail, [$;,C,B,A,$&|Acc]);
htmlize_l([$&,A,B,C,D,$;|Tail], Acc) -> htmlize_l(Tail, [$;,D,C,B,A,$&|Acc]);
htmlize_l([$&,A,B,C,D,E,$;|Tail], Acc) ->
    htmlize_l(Tail, [$;,E,D,C,B,A,$&|Acc]);
htmlize_l([$&,A,B,C,D,E,F,$;|Tail], Acc) ->
    htmlize_l(Tail, [$;,F,E,D,C,B,A,$&|Acc]);
htmlize_l([$&,A,B,C,D,E,F,G,$;|Tail], Acc) ->
    htmlize_l(Tail, [$;,G,F,E,D,C,B,A,$&|Acc]);
htmlize_l([$&,A,B,C,D,E,F,G,H,$;|Tail], Acc) ->
    htmlize_l(Tail, [$;,H,G,F,E,D,C,B,A,$&|Acc]);
htmlize_l([$&|Tail], Acc) -> htmlize_l(Tail, [$;,$p,$m,$a,$&|Acc]);
htmlize_l([$"|Tail], Acc) -> htmlize_l(Tail, [$;,$t,$o,$u,$q,$&|Acc]);
htmlize_l([X|Tail], Acc) when integer(X) -> htmlize_l(Tail,[X|Acc]);
htmlize_l([X|Tail], Acc) when binary(X) ->
    X2 = htmlize_l(binary_to_list(X)), htmlize_l(Tail, [X2|Acc]);
htmlize_l([X|Tail], Ack) when is_list(X) ->
    X2 = htmlize_l(X),
    htmlize_l(Tail, [X2|Ack]).


user_link(Username) ->
    user_link(Username, Username).

user_link(Username, Text) ->
    user_link(Username, Text, iolist).

user_link(Username, Text, iolist) ->
    erlyweb_html:a(["/users", Username], Text);
user_link(Username, Text, list) ->
    ["<a href=\"/users/", Username, "\">", Text, "</a>"].

log(Module, Line, Level, FormatFun) ->
    Func = case Level of
	       debug ->
		   info_msg;
	       info ->
		   info_msg;
	       normal ->
		   info_msg;
	       error ->
		   error_msg;
	       warn ->
		   warning_msg
	   end,
    if Level =/= debug ->
	    {Format, Params} = FormatFun(),
	    error_logger:Func("~w:~b: "++ Format ++ "~n",
			      [Module, Line | Params]);
       true ->
	    ok
    end.

replace_matches(Body, RegExp, ReplaceFun, MaxLen) ->
    {match, Matches} = regexp:matches(Body, RegExp),
    if Matches == [] ->
	    {lists:sublist(Body, MaxLen), [], 0};
       true ->
	    replace_matches1(Body, Matches, ReplaceFun, MaxLen)
    end.

replace_matches1(Body, Matches, ReplaceFun, MaxLen) ->
    {CurIdx1, Acc, RemChars3, MatchAcc, LenDiffAcc2} =
	lists:foldl(
	  fun({_Begin, _MatchLength},
	      {CurIdx, _Acc, _RemChars, _MatchAcc, LenDiffAcc} = Res)
	     when CurIdx + LenDiffAcc > MaxLen->
		  %% ignore the match if we passed MaxLen chars
		  Res;
	     ({Begin, MatchLength},
	      {CurIdx, Acc, RemChars, MatchAcc, LenDiffAcc}) ->
		  PrefixLen = Begin - CurIdx,
		  {Prefix, RemChars1} = lists:split(PrefixLen, RemChars),
		  {Match, RemChars2} = lists:split(MatchLength, RemChars1),
		  {NewStr1, NewLen} =
		      case ReplaceFun(Match) of
			  {_,_} = Res -> Res;
			  Res -> {Res, MatchLength}
		      end,
		  
		  LenDiff = NewLen - MatchLength,
		  OverFlow = (CurIdx + PrefixLen + NewLen) -
		      (MaxLen - LenDiffAcc),
		  
		  %% if we detect an overflow, we discard the match
		  {Acc1, LenDiffAcc1}
		      = if OverFlow > 0 ->
				
				%% keep the remaining prefix
				Rem = (MaxLen - LenDiffAcc) -
					(CurIdx - 1),
				Prefix1 =
				    if Rem > PrefixLen ->
					    Prefix;
				       Rem < 1 ->
					    [];
				       true ->
					    lists:sublist(Prefix, Rem)
				    end,
				{Prefix1,  LenDiffAcc - MatchLength};
			   true ->
				{[Prefix, NewStr1], LenDiffAcc + LenDiff}
			end,
		  {CurIdx + PrefixLen + MatchLength, [Acc1 | Acc], RemChars2,
		   [Match | MatchAcc], LenDiffAcc1}
	  end, {1, [], Body, [], 0}, Matches),
    RemCharsLength = MaxLen - (CurIdx1 - 1) - LenDiffAcc2,
    RemChars4 = if RemCharsLength > 0 ->
			lists:sublist(RemChars3, RemCharsLength);
		   true ->
			[]
		end,
    {[lists:reverse(Acc), RemChars4], MatchAcc, LenDiffAcc2}.
    

get_tinyurl(Url) ->
    TinyApi = "http://tinyurl.com/api-create.php?url=" ++ Url,
    case http:request(TinyApi) of
	{ok, {{_Protocol, 200, _}, _Headers, Body1}} ->
	    {["<a href=\"", Body1, "\">", Body1, "</a>"], length(Body1)};
	Res ->
	    ?Error("tinyurl error: ~p", [Res]),
	    Url
    end.


get_session_key(A) ->
    yaws_arg:get_opaque_val(A, key).

update_session(A, Usr) ->
    update_session_by_key(twoorl_util:get_session_key(A), Usr).

update_session_by_key(Key, Usr) ->
    mnesia:dirty_write(#session{key=Key,
				value=Usr}).


gravatar_icon(GravatarId) ->
    [<<"<img border=\"0\" src=\"http://www.gravatar.com/avatar.php?size=32&gravatar_id=">>,
     GravatarId, <<"\"/>">>].

gravatar_id(Email) ->
    digest2str(erlang:md5(Email)).

digest2str(Digest) ->
    [[nibble2hex(X bsr 4), nibble2hex(X band 15)] ||
	X <- binary_to_list(Digest)].

-define(IN(X,Min,Max), X >= Min, X =< Max).


nibble2hex(X) when ?IN(X, 0, 9)   -> X + $0;
nibble2hex(X) when ?IN(X, 10, 15) -> X - 10 + $a.


