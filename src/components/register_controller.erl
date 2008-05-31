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

-module(register_controller).
-compile(export_all).
-include("twoorl.hrl").

index(A) ->
    case yaws_arg:method(A) of
	'POST' ->
	    Params = yaws_api:parse_post(A),
	    {[Username, Email, Password, Password2], Errs} =
		erlyweb_forms:validate(
		  A, ["username", "email", "password", "password2"],
		  fun validate/2),
	    Errs1 = 
		if Password == Password2 ->
			Errs;
		   true ->
			Errs ++ [password_mismatch]
		end,
	    if Errs1 =/= [] ->
		    {data, Errs1};
	       true ->
		    %% todo set cookie
		    Usr = register_usr(Username, Email, Password),
		    login_controller:do_login(Usr)
	    end;
	_ ->
	    {data, []}
    end.

validate(Name, Val) ->
    case Name of
	"username" ->
	    if Val == [] ->
		    {error, {missing_field, "username"}};
	       true ->
		    case usr:find_first({username,'=',Val}) of
			undefined ->
			    {ok, twoorl_util:htmlize(Val)};
			_ ->
			    {error, {invalid_username, Val}}
		    end
	    end;
	"password" ->
	    case length(Val) >= ?MIN_PASSWORD_LENGTH of
		true ->
		    ok;
		_ ->
		    {error, password_too_short}
	    end;
	"email" ->
	    if Val == [] ->
		    {error, {missing_field, "email"}};
	       true ->
		    ok
	    end;
	_ ->
	    ok
    end.
				      
register_usr(Username, Email, Password) ->
    Usr = usr:new_with([{username, list_to_binary(Username)},
			{email, Email},
			%% not the most secure password storage method,
			%% but good enough for now
			{password, crypto:sha(Username ++ Password)}]),
    usr:save(Usr).
