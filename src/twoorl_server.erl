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
%% @doc Manages the yaws component of the application.
-module(twoorl_server).
-behaviour(gen_server).

-include_lib("yaws/include/yaws.hrl").
-include("twoorl.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, set_conf/0]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_) ->
    process_flag(trap_exit, true),
    case application:start(yaws) of
        ok -> twoorl_server:set_conf();
        Error -> {stop, Error}
    end.

set_conf() ->
    GC = yaws_config:make_default_gconf(false, "twoorl"),
    SC1 = #sconf{
        port = 5001,
        servername = "twoorl.com",
        listen = {0, 0, 0, 0},
        docroot = "www",
        appmods = [{"/", erlyweb}],
        opaque = [{"appname", "twoorl"}]
    },
    SC2 = #sconf{
        port = 5001,
        servername = "localhost",
        listen = {0, 0, 0, 0},
        docroot = "www",
        appmods = [{"/", erlyweb}],
        opaque = [{"appname", "twoorl"}]
    },
    try yaws_api:setconf(GC, [[SC1, SC2]]) of
        ok -> {ok, started};
        Errora -> {stop, Errora}
    catch
        Errorb -> {stop, Errorb}
    end.

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> application:stop(yaws), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
