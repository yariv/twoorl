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
%%
%% @author Yariv Sadan <yarivsblog@gmail.com> [http://yarivsblog@gmail.com]
%% @copyright Yariv Sadan, 2008

-module(feeds_controller).
-export([catch_all/2]).

catch_all(A, ["main", Type]) ->
    Messages = msg:find_with([{order_by, {created_on, desc}}, {limit, 20}]),
    {data, {list_to_atom(Type),
	    <<"Twoorl / Everyone">>,
	    <<"http://twoorl.com/main">>,
	    <<"Latest twoorls from everyone">>,
	    get_funs(A, Messages)}};

catch_all(A, ["users", Username, Type]) ->
     case usr:find_first({username,'=',Username}) of
	 undefined ->
	     exit({no_such_user, Username});
	 Usr ->
	     Messages = msg:find(
			  {usr_id,in, [Usr:id()]},
			  [{order_by, {created_on, desc}}, {limit, 20}]),
			 
	     {data, {list_to_atom(Type),
		     [<<"Twoorl / ">>, Username],
		     [<<"http://twoorl.com/users/">>, Username],
		     [Username, <<"'s latest twoorls">>],
		     get_funs(A, Messages)}}
     end.

catch_all(A, ["friends", Username, Type]) ->
    case usr:find_first({username,'=',Username}) of
        undefined ->
            exit({no_such_user, Username});
        Usr ->
            Ids = usr:get_timeline_usr_ids(Usr),
            Messages = msg:find(
            {usr_id,in, [Ids]},
            [{order_by, {created_on, desc}}, {limit, 20}]),
            {data, {list_to_atom(Type),
                [<<"Twoorl / ">>, Username],
                [<<"http://twoorl.com/users/">>, Username],
                [Username, <<"'s friend's latest twoorls">>],
                get_funs(A, Messages)}
            }
    end.

catch_all(A, _) ->
    catch_all(A, ["main", "rss"]);

get_funs(A, Messages) ->
    [fun(title) ->
	     [M:usr_username(), $:, 32, M:body_nolinks()];
	(description) ->
	     [M:usr_username(), $:, 32, M:body_nolinks()];
 	(htmldescription) ->
 	     [M:usr_username(), $:, 32, M:body_nolinks()];
	(pubdate) ->
	     twoorl_util:format_datetime(element(2,M:created_on()));
	(guid) ->
	     msg:get_href(A, M, absolute);
	(link) ->
	     msg:get_href(A, M, absolute)
     end || M <- Messages].
