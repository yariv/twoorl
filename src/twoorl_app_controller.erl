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

-module(twoorl_app_controller).
-export([hook/1]).
-include("twoorl.hrl").

hook(A) ->
    A1 = normalize_appmoddata(A),
    case erlyweb:get_initial_ewc({ewc, A1}) of
	{page, "/"} -> start(A1);
	{page, _} = Ewc -> Ewc;
	_Ewc -> start(A1)
    end.

start(A) ->
    {A2, LoggedIn} = auth(A),
    Appmod = yaws_arg:appmoddata(A2),
    case Appmod of
	"/api" ++ _ ->
	    {ewc, A2};
	"/" when LoggedIn == true ->
	    {ewr, home};
	_ ->
	    Appmod1 = case Appmod of
			  "/" ->
			      "/main";
			  _ ->
			      Appmod
		      end,
	    A3 = yaws_arg:appmoddata(A2, Appmod1),
	    {phased, {ewc, A3},
	     fun(_Ewc, Data, _PhasedVars) ->
		     {ewc, html_container,
		      [A3,
		       {ewc, layout, [A3, {data, Data}]}]}
	     end}
    end.

%% Populate the arg's opaque field with {session, Session} if the
%% request's "key" cookie value could be found in mnesia.
auth(A) ->
    case erlyweb_util:get_cookie("key", A) of
	undefined ->
	    {A, false};
	Val ->
	    Key = list_to_binary(Val),
	    case lookup(Key) of
		undefined ->
		    {A, false};
		Session ->
		    {yaws_arg:add_all_to_opaque(
		       A,
		       [{key, Key}, {session, Session}]), true}
	    end
    end.

lookup(Key) ->
    case mnesia:dirty_read(session, Key) of
	[] ->
	    case usr:find_first({session_key,'=',Key}) of
		undefined ->
		    undefined;
		Usr ->
		    Session = #session{key=Key, value=Usr},
		    mnesia:dirty_write(Session),
		    Session
	    end;
	[Session] ->
	    Session;
	Other ->
	    exit({unexpected_result, Other})
    end.

%% to avoid annoying Yaws inconsistencies
normalize_appmoddata(A) ->
    Val1 = 
	case yaws_arg:appmoddata(A) of
	    [] ->
		"/";
	    Val = [$/ | _] ->
		Val;
	    Val ->
		[$/ | Val]
	end,
    yaws_arg:appmoddata(A, Val1).
	
		
	    
