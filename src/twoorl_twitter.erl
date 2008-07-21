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

-module(twoorl_twitter).
-compile(export_all).
-include("twoorl.hrl").

send_tweet(Usr, Msg) ->
    Res = twitter_client:status_update(
	    Usr:twitter_username(),
	    Usr:twitter_password(),
	    [{"status", Msg:body_raw()},
	     {"source", "twoorl"}]),
    case Res of
        {error, _} -> 
            ?Warn("error sending tweet ~p ~p", [Msg:id(), Res]),
            msg:update([{twitter_status, ?TWITTER_SENT_ERR}],
		       {id, '=', Msg:id()});
	_ ->
%%	    twoorl_stats:cast({record, twitter_crosspost}),
            msg:update([{twitter_status, ?TWITTER_SENT_OK}],
		       {id, '=', Msg:id()})
    end.
