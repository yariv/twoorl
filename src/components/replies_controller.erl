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

-module(replies_controller).
-compile(export_all).
-include("twoorl.hrl").

index(A) ->
    twoorl_util:auth(
      A,
      fun(Usr) ->
	      Replies = reply:find({usr_id,'=',Usr:id()},
				   [{order_by, {created_on, desc}},
				    {limit, ?MAX_PAGE_SIZE}]),
	      MsgIds = [Reply:msg_id() || Reply <- Replies],
	      Msgs = msg:find({id,in,MsgIds}, {order_by, {created_on, desc}}),
	      {ewc, timeline, show_msgs, [A, Msgs]}
      end).
    
