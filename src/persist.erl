-module(persist).

-export([init_db/1, init_db/2]).
-export([get_pnodes/1, add_pnode/2, drop_pnode/2]).
-export([get_scenarios_list/1, get_scenario/2, get_scenario_title/2, update_scenario/5, inc_scenatio_rate/2, drop_scenario/2]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

-define(OR_EMPTY(Action), case Action of {ok, {_, Strs}} -> Strs; Any -> ?ERROR("~p", [Any]), [] end).

%
% init db ( persist:init_db(pgdb). )
%

init_db(Pool) ->
    [init_db(Pool, I) || I <- [1]].

init_db(Pool, 1) ->
    persist_pgsql:ql(Pool, [
            %
            % PNodes
            %

            "CREATE TABLE ctl_pnodes (
                    id serial,
                    addr inet NOT NULL,
                    en boolean NOT NULL DEFAULT TRUE,
                    atime timestamp DEFAULT current_timestamp
                );",
            "CREATE INDEX ctl_pnodes_en_idx ON ctl_pnodes(en);",
            "CREATE INDEX ctl_pnodes_addr_idx ON ctl_pnodes(addr);",
            "CREATE INDEX ctl_pnodes_addr_en_idx ON ctl_pnodes(addr, en);",
            "CREATE INDEX ctl_pnodes_atime_idx ON ctl_pnodes(atime);",
            %
            % Cookbook
            %
            "CREATE TABLE ctl_scenarios (
                    id serial,
                    name varchar(128),
                    rate integer DEFAULT 0,
                    params text,
                    commands text,
                    en boolean NOT NULL DEFAULT TRUE,
                    atime timestamp DEFAULT current_timestamp
                );",
            "CREATE INDEX ctl_scenarios_rate_idx ON ctl_scenarios(rate);",
            "CREATE INDEX ctl_scenarios_name_idx ON ctl_scenarios(name);",
            "CREATE INDEX ctl_scenarios_name_en_idx ON ctl_scenarios(name, en);",
            "CREATE INDEX ctl_scenarios_en_idx ON ctl_scenarios(en);"            
        ]).

%
% pnodes 
%

get_pnodes(Pool) ->
    ?OR_EMPTY(persist_pgsql:q(Pool, "SELECT * FROM ctl_pnodes WHERE en=TRUE ORDER BY addr;")).


add_pnode(Pool, IP) ->
    case persist_pgsql:qe(Pool, "SELECT id FROM ctl_pnodes WHERE addr=$1 AND en=TRUE;", [IP]) of
        {ok, {_, [{Id}]}} -> Id;     
        _Any ->
            case persist_pgsql:qe(Pool, "INSERT INTO ctl_pnodes(addr) VALUES ($1);", [IP]) of
                {ok, _Count} ->
                    case persist_pgsql:qe(Pool, "SELECT id FROM ctl_pnodes WHERE addr=$1 AND en=TRUE;", [IP]) of
                        {ok, {_, [{Id}]}} -> Id;
                        Err -> ?ERROR("Error finding pnode ~p", [Err]), 0
                    end;
                Err -> ?ERROR("Error adding pnode ~p", [Err]), 0
            end
    end.

drop_pnode(Pool, Id) ->
    persist_pgsql:qe(Pool, "UPDATE ctl_pnodes SET en=FALSE WHERE id=$1;", [Id]).

%
% scenarios
%

get_scenarios_list(Pool) ->
    case persist_pgsql:q(Pool, "SELECT id, name, rate FROM ctl_scenarios WHERE en=TRUE ORDER BY name;") of
        {ok, {_, S}} -> [[{id, Id}, 
                          {name, Name}, 
                          {rate, Rate}] || {Id, Name, Rate} <- S];
        _Any -> []
    end.

get_scenario(Pool, Id) ->
    case persist_pgsql:qe(Pool, "SELECT * FROM ctl_scenarios WHERE id=$1 AND en=TRUE;", [Id]) of
        {ok, {_, [{Id, Name, Rate, Params, Commands, _, _}]}} -> [{id, Id},
                                                                  {name, Name},
                                                                  {rate, Rate},
                                                                  {params, jsonx:decode(Params)},
                                                                  {commands, Commands}];
        _Any -> not_found
    end.

get_scenario_title(Pool, Id) ->
    case persist_pgsql:qe(Pool, "SELECT name FROM ctl_scenarios WHERE id=$1 AND en=TRUE;", [Id]) of
        {ok, {_, [{Name}]}} -> Name;
        _Any -> <<"">>
    end.
    
update_scenario(Pool, Id, Name, Params, Commands) ->
    drop_scenario(Pool, Id),   
    case persist_pgsql:qe(Pool, "INSERT INTO ctl_scenarios(name, params, commands) VALUES ($1, $2, $3);", 
            [Name, jsonx:encode(Params), Commands]) of
        {ok, _Count} -> ok;
        Err -> ?ERROR("Error update scenario ~p", [Err]), error
    end.


inc_scenatio_rate(Pool, Id) ->
    persist_pgsql:qe(Pool, "UPDATE ctl_scenarios SET rate=rate+1 WHERE id=$1;", [Id]).
 
drop_scenario(Pool, Id) ->
    persist_pgsql:qe(Pool, "UPDATE ctl_scenarios SET en=FALSE WHERE id=$1;", [Id]).

