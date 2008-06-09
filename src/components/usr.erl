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

-module(usr).
-compile(export_all).
-include("twoorl.hrl").

get_icon(Usr) ->
    get_icon(Usr, false).

get_icon(Usr, AsLink) ->
    GravatarId =
	case usr:gravatar_enabled(Usr) of
	    1 ->
		twoorl_util:gravatar_id(usr:email(Usr));
	    0 ->
		?DEFAULT_GRAVATAR_ID
	end,
    if AsLink ->
	    get_icon_link(Usr:username(), GravatarId);
       true ->
	    twoorl_util:gravatar_icon(GravatarId)
    end.

get_icon_link(Username, GravatarId) ->
    get_link(Username, twoorl_util:gravatar_icon(GravatarId)).

get_timeline_usr_ids(Usr) ->
    Followings = following:find({usr_id1,'=',Usr:id()}),
    FollowingIds = [Following:usr_id2() || Following <- Followings],
    [Usr:id() | FollowingIds].

get_feed_url(Usr, Format) ->
    [<<"/feeds/users/">>, Usr:username(), $/, Format].
    

get_link(Usr) when is_tuple(Usr) ->
    get_link(Usr:username());
get_link(Username) ->
    get_link(Username, Username).

get_link(Username, Text) ->
    get_link(Username, Text, iolist).

get_link(Username, Text, iolist) ->
    [<<"<a href=\"/">>, Username, <<"\">">>, Text, <<"</a>">>];
get_link(Username, Text, list) ->
    ["<a href=\"/", Username, "\">", Text, "</a>"].

