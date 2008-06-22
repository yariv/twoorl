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

-module(timeline_controller).
-compile(export_all).
-include("twoorl.hrl").

private() ->
    true.

index(_A) ->
    {data, undefined}.

show(A) ->
    show(A, undefined).

show(A, Usr) when not is_list(Usr), Usr =/= undefined ->
    show(A, [Usr:id()]);

show(A, UserIds) ->
    show(A, UserIds, []).

show(A, UserIds, Opts) ->
    OrderBy = {order_by, {created_on, desc}},
    
    Where = case proplists:get_value(filter_spam, Opts) of
		true ->
		    {'not', {spam,'=',1}};
		_ ->
		    undefined
	    end,
    %% this function is a prime optimization candidate
    Where1 = 
	if UserIds =/= undefined ->
		{'and', [{usr_id, in, UserIds}, Where]};
	   true ->
		Where
	end,
    Total = msg:count('*', Where1),

    {replace, 
     {ewc, paging,
      [A, fun(Limit) ->
		  msg:find(Where1, [OrderBy, Limit])
	  end,
       fun(Msgs) ->
	       {ewc, timeline, show_msgs, [A, Msgs, Opts]}
       end,
       [{total, Total}]]}}.

show_msgs(A, Msgs) ->
    show_msgs(A, Msgs, []).

show_msgs(A, Msgs, Opts) ->
    Opts1 =
	case proplists:get_value(big_first, Opts) of
	    undefined ->
		Opts;
	    true ->
		[{is_big, true} | Opts]
	end,
    if Msgs == [] ->
	    [];
       true ->
	    [{ewc, timeline, show_msg, [A, hd(Msgs), Opts1]} |
	     [{ewc, timeline, show_msg, [A, Msg, Opts]} || Msg <- tl(Msgs)]]
    end.

show_msg(A, Msg) ->
    show_msg(A, Msg, []).

show_msg(A, Msg, Opts) ->
    Username = Msg:usr_username(),
    {Icon, Userlink} =
	case proplists:get_value(hide_user, Opts) of
	    true ->
		{[], []};
	    _ ->
		GravatarId =
		    msg:get_gravatar_id(Msg),
		{usr:get_icon_link(Username, GravatarId),
		 usr:get_link(Username)}
	end,
    CreatedOn = msg:get_time_since(Msg),
    IsBig = proplists:get_value(is_big, Opts) == true,
    
    {data, {Username, Icon, Userlink,
	    Msg:body(), msg:get_href(A, Msg), CreatedOn, IsBig}}.
