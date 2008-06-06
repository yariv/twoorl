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

-module(home_controller).
-compile(export_all).
-include("twoorl.hrl").

index(A) ->
    twoorl_util:auth(
      A,
      fun(Usr) ->
	      Followings = following:find({usr_id1,'=',Usr:id()}),
	      FollowingIds = [Following:usr_id2() || Following <- Followings],
	      {FirstFollowings, _Rest} = lists:split(
					   lists:min(
					     [?MAX_FOLLOWING_BOX_SIZE,
					      length(Followings)]),
					   Followings),
	      [{data, {Usr:username(), Usr:twitter_enabled() == 1}},
	       {ewc, timeline, show, [A, [Usr:id() | FollowingIds]]}]
      end).
