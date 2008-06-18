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
%% @doc Provides a mechanism to track stats in a simple way.
-module(twoorl_stats).
-behaviour(gen_server).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.1").

-compile(export_all).

-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("stdlib/include/qlc.hrl").
-include("twoorl.hrl").

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Args) ->
    ensure_started(),
    gen_server:call(twoorl_stats, Args).

cast(Args) ->
    ensure_started(),
    gen_server:cast(twoorl_stats, Args).

bucket_key(Name, {_, {YY, MM, DD}}) ->
    bucket_key(Name, {YY, MM, DD});
bucket_key(Name, {YY, MM, DD}) ->
    lists:flatten(io_lib:format("~s-~w-~w-~w",[Name, YY, MM, DD])).

google_chart_sparkline(Records, Background) ->
    Data = lists:foldl(fun(A, "") -> A; (A, Acc) -> Acc ++ "," ++ A end,
		       "", [integer_to_list(X)|| X <- Records]),
%    Min = lists:min(Records) - 2,
%    Max = lists:max(Records) + 2,
    lists:concat(["http://chart.apis.google.com/chart?chs=400x200&cht=bvs"
		  "&chd=t:", Data, "&chco=4d89f9&chf=&chf=bg,s,",
		  Background ,"|c,s,", Background]).

google_chart_sparkline(Records) ->
    twoorl_stats:google_chart_sparkline(Records, "000000").

%% -
%% gen_server functions

init([]) ->
    dets:open_file(twoorl_stats, []).

handle_call({info}, _From, State) ->
    {reply, State, State};

handle_call({stats, Name, Range}, _From, State) when is_integer(Range) ->
    Now = calendar:datetime_to_gregorian_seconds(
	    calendar:now_to_universal_time(erlang:now())),
    Keys = [begin
		Seconds = Now - (60 * 60 * 24 * X),
		{Date, _} = calendar:gregorian_seconds_to_datetime(Seconds),
		{Date, bucket_key(Name, Date)}
	    end || X <- lists:seq(0, Range)],
    Data = [case dets:lookup(twoorl_stats, Key) of
		[{Key, Date, Count}] -> {Date, Count};
		_ -> {Date, 0}
	    end || {Date, Key} <- Keys],
    {reply, Data, State};

handle_call({graph, Name, Range}, From, State) when is_integer(Range) ->
    {_, Stats, _} = twoorl_stats:handle_call(
		      {stats, Name, Range}, From, State),
    ImgUrl = twoorl_stats:google_chart_sparkline(
	       lists:reverse([Y || {X, Y} <- Stats])),
    {reply, ImgUrl, State};

handle_call(stop, _From, State) -> {stop, normalStop, State};

handle_call(_, _From, State) -> {noreply, ok, State}.

handle_cast({record, Name}, State) ->
    {Date, _} = calendar:universal_time(),
    handle_cast({record, Name, Date}, State);
handle_cast(R= {record, Name, Date}, State) ->
    Key = bucket_key(Name, Date),
    case dets:lookup(twoorl_stats, Key) of
        [_] -> dets:update_counter(twoorl_stats, Key, {3, 1});
        _ -> dets:insert(twoorl_stats, [{Key, Date, 1}])
    end,
    {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

ensure_started() ->
    case whereis(twoorl_stats) of 
        X when is_pid(X) -> ok;
        _ -> twoorl_stats:start()
    end.


