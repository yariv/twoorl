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

cookie(Key, Val) ->
    yaws_api:setcookie(Key, Val, "/", "Wed 01-01-2020 00:00:00 GMT").

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
    {Type, Val} =
	if Diff < 60 ->
		{seconds_ago, Diff};
	   Diff < 3600 ->
		{minutes_ago, round(Diff/60)};
	   Diff < 86400 ->
		{hours_ago, round(Diff/3600)};
	   true ->
		{days_ago, round(Diff/86400)}
	end,
    {Type, integer_to_list(Val)}.

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


log(Module, Line, Level, FormatFun) ->
    Func = case Level of
	       debug ->
		   % info_msg;
		   undefined;
	       info ->
		   info_msg;
	       normal ->
		   info_msg;
	       error ->
		   error_msg;
	       warn ->
		   warning_msg
	   end,
    if Func == undefined ->
	    ok;
	true ->
	    {Format, Params} = FormatFun(),
	    error_logger:Func("~w:~b: "++ Format ++ "~n",
			      [Module, Line | Params])
    end.

%% @doc Replace all the Body's substrings matching the RegExp
%% or Matches list using the
%% ReplaceFun function and without exceeding MaxLen characters.
%%
%% ReplaceFun takes one parameter: the substring to
%% be matched. It returns a tuple of the form {Replacement,
%% EffectiveReplacement} or just a replacement string.
%% Replacement is the new substring. EffectiveReplacement (optional)
%% is the "real" value of the replacement, which will be used for
%% calculating the replacement's length and which will be returned in the
%% function's return value. (EffectiveReplacement helps deal with links.)
%%
%% The return value is {Result, Changes}.
%% Result is the new string with the substitutions applied (and whose length
%% doesn't exceed MaxLen). Changes is a list of all {Match, Replacement} pairs
%% where Match is the substring matching RegExp and Replacement is its
%% replacement.
%%
%% @spec replace_matches(Body::string(),
%%  RegExp::regexp() | [{integer(), integer()}],
%%  ReplaceFun::function(), MaxLen::integer()) ->
%%    {NewBody::iolist(), [{string(), string()}]}
replace_matches(Body, RegExp, ReplaceFun, MaxLen) when is_tuple(RegExp) ->
    {match, Matches} = regexp:matches(Body, RegExp),
    replace_matches(Body, Matches, ReplaceFun, MaxLen);
replace_matches(Body, Matches, ReplaceFun, MaxLen) ->
    if Matches == [] ->
	    {lists:sublist(Body, MaxLen), []};
       true ->
	    replace_matches1(Body, Matches, ReplaceFun, MaxLen)
    end.
replace_matches1(Body, Matches, ReplaceFun, MaxLen) ->
    {CurIdx1, Acc, RemChars3, MatchAcc2, LenDiffAcc2} =
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
		  {Replacement1, EffectiveReplacement1,
		   EffectiveReplacementLen} =
		      case ReplaceFun(Match) of
			  {Replacement2, EffectiveReplacement2} ->
			      {Replacement2, EffectiveReplacement2,
			       length(EffectiveReplacement2)};
			  Replacement2 ->
			      {Replacement2, Replacement2,
			       length(Replacement2)}
		      end,
		  
		  LenDiff = EffectiveReplacementLen - MatchLength,
		  OverFlow = (CurIdx + PrefixLen + EffectiveReplacementLen) -
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
				{[Prefix, Replacement1], LenDiffAcc + LenDiff}
			end,
		  {CurIdx + PrefixLen + MatchLength, [Acc1 | Acc], RemChars2,
		   [{Match, EffectiveReplacement1} | MatchAcc], LenDiffAcc1}
	  end, {1, [], Body, [], 0}, Matches),
    RemCharsLength = MaxLen - (CurIdx1 - 1) - LenDiffAcc2,
    RemChars4 = if RemCharsLength > 0 ->
			lists:sublist(RemChars3, RemCharsLength);
		   true ->
			[]
		end,
    {[lists:reverse(Acc), RemChars4], MatchAcc2}.
    

get_tinyurl("http://tinyurl.com/" ++ Rest = Url) when length(Rest) < 7 ->
    get_tinyurl1(Url);
get_tinyurl(Url) when length(Url) < 26 -> get_tinyurl1(Url);
get_tinyurl(Url) ->
    TinyApi = "http://tinyurl.com/api-create.php?url=" ++ Url,
    case http:request(TinyApi) of
	{ok, {{_Protocol, 200, _}, _Headers, Body1}} ->
	    get_tinyurl1(Body1);
	Res ->
	    ?Error("tinyurl error: ~p", [Res]),
	    Url
    end.

get_tinyurl1(Body) ->
    {["<a href=\"", Body, "\">", Body, "</a>"], Body}.



get_session_key(A) ->
    yaws_arg:get_opaque_val(A, key).

update_session(A, Usr) ->
    Key = twoorl_util:get_session_key(A),
    update_session(A, Usr, Key).

update_session(A, Usr, Key) ->
    NewSession = #session{key=Key,
			  value=Usr},
    mnesia:dirty_write(NewSession),
    Opaque1 = 
	lists:map(fun({session, _}) -> {session, NewSession};
		     (Other) -> Other
		  end, yaws_arg:opaque(A)),
    yaws_arg:opaque(A, Opaque1).


gravatar_icon(GravatarId) ->
    [<<"<img alt=\"gravatar\" width=\"32\" height=\"32\" class=\"gravatar\" "
      "src=\"http://www.gravatar.com/avatar.php?"
      "size=32&amp;gravatar_id=">>,
     GravatarId, <<"\"/>">>].

gravatar_id(Email) ->
    digest2str(erlang:md5(Email)).

digest2str(Digest) ->
    [[nibble2hex(X bsr 4), nibble2hex(X band 15)] ||
	X <- binary_to_list(Digest)].

-define(IN(X,Min,Max), X >= Min, X =< Max).


nibble2hex(X) when ?IN(X, 0, 9)   -> X + $0;
nibble2hex(X) when ?IN(X, 10, 15) -> X - 10 + $a.


iolist_fun(Rec) ->
    fun(Field) ->
	    erlydb_base:field_to_iolist(Rec:Field())
    end.

iolist_fun(Rec, Fun) ->
    fun(Field) ->
	    case Fun(Field) of
		default ->
		    erlydb_base:field_to_iolist(Rec:Field());
		Other ->
		    Other
	    end
    end.



%% Used for RSS formatting
%% Reference: http://cyber.law.harvard.edu/rss/rss.html
format_datetime({Date = {Year, Month, Day}, {Hour, Minute, Second}}) ->
    Daynum = calendar:day_of_the_week(Date),
    [day(Daynum), $,, 32, itos(Day), 32, mon(Month), 32,
     itos(Year), 32, itos(Hour, 2), $:, itos(Minute, 2), $:, itos(Second, 2),
     <<" GMT">>].

itos(N) ->
    integer_to_list(N).
itos(N, Length) ->
    Str = itos(N),
    Diff =  Length - length(Str),
    [lists:duplicate(Diff, $0), Str].

day(N) ->
    case N of
	1 -> <<"Mon">>;   
	2 -> <<"Tue">>;
	3 -> <<"Wed">>;
	4 -> <<"Thu">>;
	5 -> <<"Fri">>;
	6 -> <<"Sat">>;
	7 -> <<"Sun">>
		 end.

mon(N) ->
    case N of
	1 -> <<"Jan">>;
	2 -> <<"Feb">>;    
	3 -> <<"Mar">>;
	4 -> <<"Apr">>;
	5 -> <<"May">>;
	6 -> <<"Jun">>;
	7 -> <<"Jul">>;
	8 -> <<"Aug">>;
	9 -> <<"Sep">>;
	10 -> <<"Oct">>;
	11 -> <<"Nov">>;
	12 -> <<"Dec">>
		  end.


get_feed_link(Url, Format) ->
    erlyweb_html:a([Url], Format, [{"class", "feed_link"}]).

with_bundle(A, Data) ->
    {data, {get_bundle(A), Data}}.

%% codes taken from http://www.loc.gov/standards/iso639-2/php/code_list.php
bundles() ->
    [{<<"eng">>, <<"English">>, twoorl_eng},
     {<<"spa">>, <<"Español">>, twoorl_spa},
     {<<"dk">>, <<"Dansk">>, twoorl_dk},
     {<<"deu">>, <<"Deutsch">>, twoorl_deu},
     {<<"fra">>, <<"Français">>, twoorl_fra},
     {<<"gre">>, <<"Greek">>, twoorl_greek},
     {<<"ita">>, <<"Italiano">>, twoorl_ita},
     {<<"kor">>, <<"한국어">>, twoorl_kor},
      {<<"pol">>, <<"Polski">>, twoorl_pol},
      {<<"por">>, <<"Português Brasileiro">>, twoorl_por_br},
      {<<"ru">>, <<"Русский">>, twoorl_rus},
      {<<"sv">>, <<"Svenska">>, twoorl_sv},
      {<<"ch">>, <<"简体中文">>, twoorl_zh_cn}].

get_bundle(A) ->
    Module1 =
	case erlyweb_util:get_cookie("lang", A) of
	    undefined ->
		twoorl_eng;
	    [] ->
		twoorl_eng;
	    Lang ->
		Lang1 = list_to_binary(Lang),
		case lists:keysearch(Lang1, 1, bundles()) of
		    false ->
			?Warn("undefined language: ~p ~p", 
			      [get_usr(A), Lang1]),
			twoorl_eng;
		    {value, {_, _, Module}} -> Module
		end
	end,
    fun(StrId) ->
	    %% Some values may not have been translated from English.
	    %% We catch such exceptions and fall back on the english
	    %% translation.
	    case catch Module1:bundle(StrId) of
		{'EXIT', Err} ->
		    case Module1 of
			twoorl_eng ->
			    %% this is not supposed to happen
			    exit(Err);
			_ ->
			    twoorl_eng:bundle(StrId)
		    end;
		Other ->
		    Other
	    end
    end.

i18n(A, Val) ->
    F = get_bundle(A), F(Val).
	    

delete_sessions() ->
    Sessions = mnesia:dirty_match_object(#session{_ = '_'}),
    lists:foreach(fun mnesia:dirty_delete_object/1, Sessions).
