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

-module(users_controller).
-compile(export_all).

catch_all(A, [Username]) ->
    case usr:find_first({username,'=',Username}) of
	undefined ->
	    exit({no_such_user, Username});
	Usr ->
	    ToFollow =
		case twoorl_util:get_usr(A) of
		    undefined = Val -> Val;
		    Me ->
			case Me:id() == Usr:id() of
			    true ->
				undefined;
			    _ ->
				Following = following:find_first(
					      {'and',
					       [{usr_id1,'=',Me:id()},
						{usr_id2,'=',Usr:id()}]}),
				Following == undefined
			end
		end,
	    [{data, {Username, ToFollow}},
	     {ewc, timeline, show, [A, [Usr:id()],
				    [{big_first, true},
				     {hide_user, true}]]}]
    end.
