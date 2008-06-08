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
%% Copyright Nick Gerakines, 2008
%%
%% @author Nick Gerakines <nick@gerakines.net> [http://blog.socklabs.com/]
%% @copyright Nick Gerakines, 2008

-module(feed_controller).
-compile(export_all).


catch_all(A, ["main"]) ->
    Messages = msg:find_with([{order_by, {created_on, desc}}, {limit, 20}]),
    {data, {Messages}};


catch_all(A, ["user", Username]) ->
    case usr:find_first({username,'=',Username}) of
	undefined ->
	    exit({no_such_user, Username});
	Usr ->
        Messages = msg:find({usr_id,in, [Usr:id()]}, [{order_by, {created_on, desc}}, {limit, 20}]),
	    {data, {Username, Usr, Messages}}
    end.
