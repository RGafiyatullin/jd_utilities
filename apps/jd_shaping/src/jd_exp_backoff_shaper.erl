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

-module(jd_exp_backoff_shaper).
-behaviour(gen_server).
-export([
		start_link/1,
		shape/4
	]).
-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
	]).
-export([
		worker_init/2
	]).

%%% %%% %%%
%%% API %%%
%%% %%% %%%
-type concurrency() :: pos_integer().

-spec start_link( concurrency() ) -> {ok, pid()} | {error, Reason :: term()}.
start_link( Concurrency ) ->
	gen_server:start_link( ?MODULE, {Concurrency}, [] ).

shape( Shaper, ActionF, NextReqIn, TotalTimeout ) ->
	case client_shape_req_loop( Shaper, ActionF, 0, NextReqIn, TotalTimeout ) of
		{ok, Result} -> Result;
		{error, Error} -> error(Error);
		{throw, Throw} -> throw(Throw);
		{exit, Exit} -> exit(Exit)
	end.

%%% %%%%%%%%%% %%%
%%% gen_server %%%
%%% %%%%%%%%%% %%%

-record( workers, {
		count = 0 :: non_neg_integer(),
		entries = dict:new() :: dict()
	} ).
-record(s, {
		running_max = throw({not_nil, 's.running_max'}) :: concurrency(),
		workers = #workers{} :: #workers{}
	}).

init( { Concurrency } ) ->
	{ok, #s{
		running_max = Concurrency
	}}.
handle_call( {req, ActionF}, ReplyTo, State ) ->
	case srv_is_busy( State ) of
		true -> {reply, {error, busy}, State};
		false ->
			NewState = srv_spawn_request(
						ActionF, ReplyTo, State ),
			{noreply, NewState}
	end;
handle_call( Req, _From, State ) -> {stop, {badarg, Req}, badarg, State }.
handle_cast( Msg, State ) -> {stop, {badarg, Msg}, State }.

handle_info( {'DOWN', _, process, WorkerPid, _}, State = #s{ workers = Workers } ) ->
	{_, NewWorkers} = srv_unregister_worker( WorkerPid, Workers ),
	{noreply, State #s{ workers = NewWorkers }};

handle_info( Msg, State ) -> handle_cast( Msg, State ).
terminate( _Reason, _State ) -> ok.
code_change( _Vsn, _Extra, State ) -> {stop, {not_implemented, code_change}, State}.

%%% %%%%%% %%%
%%% Worker %%%
%%% %%%%%% %%%
worker_init( ActionF, ReplyTo ) ->
	proc_lib:init_ack( {ok, self()} ),
	ReplyWith = 
		try
			{ok, ActionF()}
		catch
			ErrorType:ErrorReason -> {ErrorType, ErrorReason}
		end,
	gen_server:reply( ReplyTo, ReplyWith ),
	ok.

%%% %%%%%%%% %%%
%%% Internal %%%
%%% %%%%%%%% %%%
client_shape_req_loop( _, _, Sleep, _, TimeLeft ) when Sleep > TimeLeft -> {error, timeout};
client_shape_req_loop( Shaper, ActionF, Sleep, NextReqIn, TimeLeft ) when Sleep =< TimeLeft ->
	ok = do_sleep( Sleep ),
	case gen_server:call( Shaper, {req, ActionF}, infinity ) of
		{ok, Result} -> {ok, Result};
		{error, busy} -> client_shape_req_loop( Shaper, ActionF, NextReqIn, NextReqIn * 2, TimeLeft - Sleep );
		{error, Reason} -> {error, Reason};
		{throw, Reason} -> {throw, Reason};
		{exit, Reason} -> {exit, Reason}
	end.

srv_is_busy( #s{ running_max = Max, workers = Workers } ) -> 
	Current = srv_workers_count( Workers ),
	Max =< Current.

srv_spawn_request( ActionF, ReplyTo, State = #s{
		workers = OldWorkers
	} ) ->
		{ok, WorkerPid} = proc_lib:start( ?MODULE, worker_init, [ ActionF, ReplyTo ] ),
		_WorkerMonRef = erlang:monitor( process, WorkerPid ),
		State #s{
			workers = srv_register_worker( WorkerPid, ReplyTo, OldWorkers )
		}.

%%% %%% %%%

srv_workers_count( #workers{ count = C } ) -> C.
srv_register_worker( WorkerPid, ReplyTo, W = #workers{
		count = Count,
		entries = Entries
	} ) ->
		W #workers{
			count = Count + 1,
			entries = dict:store( WorkerPid, ReplyTo, Entries )
		}.
srv_unregister_worker( WorkerPid, W = #workers{
		count = Count,
		entries = Entries
	} ) ->
		ReplyTo = dict:fetch( WorkerPid, Entries ),
		NewEntries = dict:erase( WorkerPid, Entries ),
		NewW = W #workers{
			count = Count - 1,
			entries = NewEntries
		},
		{ReplyTo, NewW}.


do_sleep( 0 ) -> ok;
do_sleep( Ms ) -> timer:sleep( Ms ), ok.
