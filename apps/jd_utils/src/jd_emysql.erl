% Copyright 2013 and onwards Roman Gafiyatullin
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
% 
% See the NOTICE file distributed with this work for additional information regarding copyright ownership.
% 

-module (jd_emysql).
-behaviour(gen_server).

-export([
	start_link/3,
	select/5,
	modify/5
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include_lib("jd_utils/include/logging.hrl").
-include_lib("jd_utils/include/gen_server_spec.hrl").
-include_lib("emysql/include/emysql.hrl").

-record(s, {
		svq :: pid()
	}).

-spec start_link( 
	PoolName :: atom(), 
	EmyCfg :: proplists:proplist(), 
	SVCfg :: proplists:proplist()
) -> {ok, pid()}.
start_link(PoolName, EmyCfg, SVCfg) ->
	{ok, _} = gen_server:start_link(?MODULE, {PoolName, EmyCfg, sv_cfg_apply_defaults(SVCfg)}, []).

select( Pool, QSelectIOL, QArgs, OnNonEmptySet, OnEmptySet ) ->
	QSelect = iolist_to_binary( [ QSelectIOL ] ),
	TS0 = ts(),
	report( [{pool, Pool}, select, {q, QSelect}, {a, QArgs}] ),
	sv_run( Pool, fun() ->
		case emysql:execute( Pool, QSelect, QArgs ) of
			#result_packet{
				rows = []
			} -> 
				TS1 = ts(),
				report([{pool, Pool}, select, {q, QSelect}, {a, QArgs},
						{rows, 0}, {dt, TS1 - TS0}]),
				OnEmptySet();
			#result_packet{
				rows = Rows
			} ->
				TS1 = ts(),
				report([{pool, Pool}, select, {q, QSelect}, {a, QArgs},
						{rows, length(Rows)}, {dt, TS1 - TS0}]),
				OnNonEmptySet( Rows )
		end
	end).

modify( Pool, QInsertIOL, QArgs, OnOneAffectedRow, OnNoAffectedRows ) ->
	QInsert = iolist_to_binary( [ QInsertIOL ] ),
	TS0 = ts(),
	report( [{pool, Pool}, modify, {q, QInsert}, {a, QArgs}] ),
	sv_run( Pool, fun() ->
		case emysql:execute( Pool, QInsert, QArgs ) of
			#ok_packet{ affected_rows = 1 } ->
				TS1 = ts(),
				report( [{pool, Pool}, modify, {q, QInsert}, {a, QArgs},
						one_affected_row, {dt, TS1 - TS0}] ),
				OnOneAffectedRow();
			#ok_packet{ affected_rows = 2 } ->
				TS1 = ts(),
				report( [{pool, Pool}, modify, {q, QInsert}, {a, QArgs},
						one_affected_row_update, {dt, TS1 - TS0}] ),
				OnOneAffectedRow();
			EmyResult = #ok_packet{} ->
				TS1 = ts(),
				report( [{pool, Pool}, modify, {q, QInsert}, {a, QArgs},
						no_affected_rows, {dt, TS1 - TS0}] ),
				OnNoAffectedRows( EmyResult );
			EmyResult = #error_packet{} ->
				TS1 = ts(),
				report( [{pool, Pool}, modify, {q, QInsert}, {a, QArgs},
						error_packet, {dt, TS1 - TS0}] ),
				EmyResult
		end
	end).

%%% %%%%%%%%%% %%%
%%% gen_server %%%
%%% %%%%%%%%%% %%%

init({PoolName, EmyCfg, SVCfg}) ->
	case emy_pool_find( PoolName ) of
		undefined ->
			ok = emy_pool_add( PoolName, EmyCfg );
		_ ->
			ok = emy_pool_rm( PoolName ),
			ok = emy_pool_add( PoolName, EmyCfg )
	end,
	{ok, SVQSrv} = sv_queue:start_link( PoolName, sv_queue:parse_configuration(SVCfg) ),
	{ok, #s{
		svq = SVQSrv
	}}.

handle_call(Request, _From, State = #s{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #s{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #s{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ignore.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% %%%%%%%% %%%
%%% Internal %%%
%%% %%%%%%%% %%%

sv_run( Pool, Fun ) ->
	case erlang:whereis( Pool ) of
		SV when is_pid( SV ) ->
			case sv:run( Pool, Fun ) of
				{ok, Result} -> Result;
				{error, SvErr} -> error({sv_error, SvErr})
			end;
		undefined -> exit( pool_not_found )
	end.

% sv_run( P, Fun ) ->
% 	?log_error( [?MODULE, sv_run, {pool, P}] ),
% 	Fun().

ts() -> jd_cal:get_timestamp_microseconds().
report( Report ) -> jd_log:report_el( debug, [ ?MODULE | Report ] ).

emy_pool_find( PoolName ) -> emysql_conn_mgr:find_pool( PoolName, emysql_conn_mgr:pools() ).
emy_pool_rm( PoolName ) -> ok = emysql:remove_pool( PoolName ).
emy_pool_add( PoolName, EmyCfg ) ->
	ok = emysql:add_pool(
			PoolName, proplists:get_value( pool_size, EmyCfg ),
			proplists:get_value(user, EmyCfg), proplists:get_value( password, EmyCfg ),
			proplists:get_value(host, EmyCfg), proplists:get_value( port, EmyCfg ),
			proplists:get_value(database, EmyCfg), proplists:get_value( encoding, EmyCfg )
		).

sv_cfg_apply_defaults( C ) ->
	[
		{hz, proplists:get_value( hz, C, 1000 )},
		{rate, proplists:get_value( rate, C, 100 )},
		{token_limit, proplists:get_value( token_limit, C, 150 )},
		{size, proplists:get_value( size, C, 10 )},
		{concurrency, proplists:get_value( concurrency, C, 10 ) }
	].

