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

-module(twitter).
-compile(export_all).
-include("twoorl.hrl").

post(Url, Username, Password, Params) when is_binary(Username) ->
    post(Url, binary_to_list(Username), Password, Params);

post(Url, Username, Password, Params) when is_binary(Password) ->
    post(Url, Username, binary_to_list(Password), Params);

post(Url, Username, Password, Params) ->
    Encoded = binary_to_list(
		base64:encode(Username ++ ":" ++ Password)),
    Headers =
	[{"Authorization", "Basic " ++ Encoded}],
    ContentType = "application/x-www-form-urlencoded",
    Body = twoorl_util:join([[Field,$=,yaws_api:url_encode(Val)] || {Field,Val} <- Params], "&"),
    http:request(
      post,
      {Url, Headers, ContentType, iolist_to_binary(Body)}, [], []).

update(Username, Password, Status) ->
    post("http://twitter.com/statuses/update.json", Username, Password, [{"status", Status}]).

verify_credentials(Username, Password) ->
    case post("http://twitter.com/account/verify_credentials.json", Username, Password, []) of
	{ok, {{_, Status, _}, _Headers, _Body}} ->
	    case Status of
		200 ->
		    ok;
		401 ->
		    {error, unauthorized};
		_ ->
		    {error, {unexpected_status, Status}}
	    end;
	Res ->
	    {error, Res}
    end.
