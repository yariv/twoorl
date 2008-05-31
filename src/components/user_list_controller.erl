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

-module(user_list_controller).
-compile(export_all).
-include("twoorl.hrl").

private() ->
    true.

show_related(A, Username, IsFollowing) ->
    case usr:find_first({username,'=',Username}) of
	undefined ->
	    exit({no_such_user, Username});
	Usr ->
	    {Field1, Field2} =
		if IsFollowing ->
			{usr_id1, usr_id2};
		   true ->
			{usr_id2, usr_id1}
		end,
	    Followings = following:find(
			   {Field1,'=',Usr:id()},
			   {limit, ?MAX_PAGE_SIZE}),
	    Usr2Ids =
		[following:Field2(Following) || Following <- Followings],
	    Usrs = usr:find({id,in,Usr2Ids}),
	    {data, {Username, IsFollowing,
		    [twoorl_util:user_link(Usr2:username()) ||
			Usr2 <- Usrs]}}
    end.
    
