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
    [<<"<a href=\"/users/">>, Username, <<"\">">>, Text, <<"</a>">>].
