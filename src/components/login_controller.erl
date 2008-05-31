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

-module(login_controller).
-compile(export_all).
-include("twoorl.hrl").

index(A) ->
    case yaws_arg:method(A) of
	'POST' ->
	    {[Usr, Password], Errs} =
		erlyweb_forms:validate(
		  A,
		  ["username", "password"],
		  fun(Field, Val) ->
			  case Val of
			      [] -> {error, {missing_field, Field}};
			      _ ->
				  case Field of
				      "username" ->
					  get_usr(Val);
				      _ ->
					  ok
				  end
			  end
		  end),
	    Errs1 = 
		if Errs == [] ->
			Hash = crypto:sha([usr:username(Usr), Password]),
			case usr:password(Usr) of
			    Hash ->
				[];
			    _ ->
				[invalid_login]
			end;
		   true ->
			Errs
		end,
	    if Errs1 == [] ->
		    do_login(Usr);
	       true ->
		    {data, Errs1}
	    end;
	_ ->
	    {data, []}
    end.

get_usr(Username) ->
    Usr = usr:find_first({username,'=',Username}),
    if Usr == undefined ->
	    {error, invalid_login};
       true ->
	    {ok, Usr}
    end.

do_login(Usr) ->
    Key = twoorl_util:gen_key(),
    mnesia:dirty_write(#session{key=Key,
				value=Usr}),
    usr:update([{session_key, Key}], {id,'=',usr:id(Usr)}),
    {response, [yaws_api:setcookie("key", Key), ewr]}.
