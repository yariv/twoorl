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

-module(msg).
-compile(export_all).
-include("twoorl.hrl").

get_time_since(Msg) ->
    twoorl_util:get_time_since(Msg:created_on()).

get_href(A, Msg) ->
    get_href(A, Msg, relative).

get_href(_A, Msg, relative) ->
    [<<"/twoorls/">>, Msg:usr_username(), $/,
     integer_to_list(Msg:id())];

get_href(A, Msg, absolute) ->
    [<<"http://">>, yaws_headers:host(A),
     get_href(A, Msg, relative)].

process_raw_body(Body) ->
    Body1 = twoorl_util:htmlize(Body),
    {Body2, BodyNoLinks} = add_tinyurl_links(Body1),
    {Body3, RecipientNames} = add_reply_links(lists:flatten(Body2)),
    {lists:flatten(Body3), lists:flatten(BodyNoLinks), RecipientNames}.

add_reply_links(Body) ->
    %% regexp:parse("@[A-Za-z0-9_]+")
    Re = {concat,64,
            {pclosure,{char_class,[95,{48,57},{97,122},{65,90}]}}},
    {Body1, Changes} = 
	replace_matches(
	  Body, Re, fun([_ | Name] = Val) ->
			    {usr:get_link(Name, Val, list), Val}
		    end),
    {Body1, [Match || {[_|Match], _Replacement} <- Changes]}.
	      
add_tinyurl_links(Body) ->
    %% regexp:parse("http://[^\s]+")
    Re = {concat,
	  {concat,
	   {concat,
	    {concat,{concat,{concat,{concat,104,116},116},112},58},
	    47},
	   47},
	  {pclosure,{comp_class," "}}},
    {match, Matches} = regexp:matches(Body, Re),

    %% Perform two passes: once for the web formatting (anchor tags included)
    %% and once for RSS formatting (no anchor tags).
    %% Remember the tinyurl replacements from the first pass in the
    %% second pass.
    {Body1, Changes1} =
	replace_matches(Body, Matches, fun twoorl_util:get_tinyurl/1),
    {Body2, _Changes2} =
	replace_matches(
	  Body, Matches, fun(Url) ->
				 proplists:get_value(Url, Changes1)
			 end),
    {Body1, Body2}.


replace_matches(Body, Matches, Fun) ->
    twoorl_util:replace_matches(Body, Matches, Fun, ?MAX_TWOORL_LEN).

get_gravatar_id(Msg) ->
    case Msg:usr_gravatar_enabled() of
	1 ->
	    case Msg:usr_gravatar_id() of
		undefined ->
		    ?DEFAULT_GRAVATAR_ID;
		Other ->
		    Other
	    end;
	0 ->
	    ?DEFAULT_GRAVATAR_ID
    end.

    
