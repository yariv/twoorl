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

start(_Type, _Args) ->
    twoorl_sup:start_link([]).

start_phase(mysql, _, _) ->
    {ok, DBConfig} = application:get_env(twoorl, dbconns),
    [mysql_connect(PoolSize, Hostname, User, Password, Database)
     || {Hostname, User, Password, Database, PoolSize} <- DBConfig],
    ok;

start_phase(compile, _, _) ->
    twoorl:compile(),
    ok;

%% Having the mnesia store on a separate but connected node with a module
%% to handle its maintenance would move a lot of this foo out of the
%% application stack. Eventually that really needs to happen. -- nkg
start_phase(mnesia, _, _) ->
    %% Mnesia should have been started already, because of that the schema
    %% is in memory if the schema doesn't already exist on disc. If so we
    %% change the type so that it writes to the mnesia dir we set. -- nkg
    case mnesia:table_info(schema, storage_type) of
        ram_copies -> 
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ ->
            ok
    end,
    ExistingTables = mnesia:system_info(tables) -- [schema],
    {ok, Tables} = application:get_env(twoorl, tables),
    [create_table(Table) ||
	Table <- Tables, not lists:member(Table, ExistingTables)],
    ok.

create_table(session) ->
    mnesia:create_table(session, [{attributes, record_info(fields, session)}]),
    ok.

mysql_connect(PoolSize, Hostname, User, Password, Database) ->
    erlydb:start(
      mysql, [{hostname, Hostname},
	      {username, User},
	      {password, Password},
	      {database, Database},
	      {logfun, fun twoorl_util:log/4}]),
    lists:foreach(
      fun() ->
	      mysql:connect(erlydb_mysql, Hostname, undefined, User, Password,
			    Database, true)
      end, lists:seq(1, PoolSize - 1)).

compile() ->
    compile([]).

compile_dev() ->
    compile([{auto_compile, true}]).

compile_update() ->
    compile([{last_compile_time, auto}]).

compile(Opts) ->
    erlyweb:compile(compile_dir(default),
		    [{erlydb_driver, mysql}, {erlydb_timeout, 20000} | Opts]).

compile_dir(auto) ->
    {ok, CWD} = file:get_cwd(), CWD;
compile_dir(default) ->
    ?APP_PATH;
compile_dir(appconfig) ->
    {ok, CDir} = application:get_env(twoorl, compile_dir),
    CDir;
compile_dir(Dir) ->
    Dir.

%% --- The rest of these functions will be deprecated --- %%

start() ->
    application:start(inets),
    init_mnesia(),
    TablesInfo = [{session, [{attributes, record_info(fields, session)}]}],
    create_mnesia_tables(TablesInfo),
    init_mysql(),
    compile().

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
		  {logfun, fun log/4}]),
    lists:foreach(
      fun(_) ->
	      mysql:connect(erlydb_mysql, ?DB_HOSTNAME, undefined,
			    ?DB_USERNAME, ?DB_PASSWORD, ?DB_DATABASE, true)
      end, lists:seq(1, ?DB_POOL_SIZE)).

log(Module, Line, Level, FormatFun) ->
    twoorl_util:log(Module, Line, Level, FormatFun).
