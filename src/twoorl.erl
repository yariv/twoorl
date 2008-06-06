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

-module(twoorl).
-compile(export_all).
-include("twoorl.hrl").
-include("twoorl_app.hrl").

start() ->
    application:start(inets),
    init_mnesia(),
    TablesInfo = [{session, [{attributes, record_info(fields, session)}]}],
    create_mnesia_tables(TablesInfo),
    init_mysql(),
    compile().

compile() ->
    compile([]).

compile_dev() ->
    compile([{auto_compile, true}]).

compile_update() ->
    compile([{last_compile_time, auto}]).

compile(Opts) ->
    erlyweb:compile(?APP_PATH,
		    [{erlydb_driver, mysql}, {erlydb_timeout, 20000} | Opts]).

init_mnesia() ->
    ?L("creating schema"),
    case mnesia:create_schema([node()]) of
        {error, {_,{already_exists, _}}} ->
            ?L("schema already exists");
	{error, Err} ->
            ?L("schema creation error, aborting"),
            exit(Err);
        ok ->
            ok
    end,
    case mnesia:start() of
        {error, Err1}  ->
            ?L("error starting mnesia"),
            exit(Err1);
        ok -> ok
    end.

create_mnesia_tables(TablesInfo) ->
    ExistingTables = mnesia:system_info(tables) -- [schema],
    TablesToCreate = [TableInfo || TableInfo = {Name, _Atts} <- TablesInfo,
				   not lists:member(Name, ExistingTables)],
    lists:foreach(
      fun({TableName, Atts}) ->
	      create_table(TableName, Atts)
      end, TablesToCreate),
    
    ?L({"waiting for tables", ExistingTables}),
    case mnesia:wait_for_tables(ExistingTables, 20000) of
	{timeout, RemainingTabs} ->
	    exit({mnesia_timeout, RemainingTabs});
        ok ->
            ok
    end.

create_table(Table, Def) ->
    ?L("creating table: " ++ atom_to_list(Table)),
    case mnesia:create_table(Table, Def) of
        {atomic, ok} -> ok;
        {aborted, Err2} ->
            ?L("create table error"),
            exit(Err2)
    end.


init_mysql() ->
    erlydb:start(mysql,
		 [{hostname, ?DB_HOSTNAME},
		  {username, ?DB_USERNAME}, {password, ?DB_PASSWORD},
		  {database, ?DB_DATABASE},
		  {logfun, fun twoorl_util:log/4}]),
    lists:foreach(
      fun(_) ->
	      mysql:connect(erlydb_mysql, ?DB_HOSTNAME, undefined,
			    ?DB_USERNAME, ?DB_PASSWORD, ?DB_DATABASE, true)
      end, lists:seq(1, ?DB_POOL_SIZE)).

