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

-module(twoorls_controller).
-compile(export_all).

catch_all(_A, [_Username, Id]) ->
    %% currently, we ignore the username, but we keep it as a parameter
    %% for future user-based sharding
    case msg:find_id(list_to_integer(Id)) of
	undefined ->
	    exit({not_found, Id});
	Msg ->
	    {data, {twoorl_util:user_link(Msg:usr_username()),
		    Msg:body(),
		    Msg:get_time_since()}}
    end.
    
    
    
