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

-module (jd_hooks_compiler).
-behaviour (gen_server).

-export([
	start_link/0,
	rebuild_all/0,
	notify_hook_handlers_updated/1,
	enable_rebuild_all/0,
	disable_rebuild_all/0
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

-record(s, {
		auto_rebuild_all = false :: boolean()
	}).

-spec start_link() -> {ok, pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

enable_rebuild_all() -> gen_server:call( ?MODULE, enable_rebuild_all ).
disable_rebuild_all() -> gen_server:call( ?MODULE, disable_rebuild_all ).
notify_hook_handlers_updated( HookID ) -> gen_server:call( ?MODULE, {hook_handlers_updated, HookID} ).

rebuild_all() -> gen_server:call( ?MODULE, rebuild_all ).

%%% %%%%%%%%%% %%%
%%% gen_server %%%
%%% %%%%%%%%%% %%%

init({}) -> {ok, #s{}}.

handle_call( disable_rebuild_all, _From, State = #s{ auto_rebuild_all = false } ) ->
	{reply, ok, State};
handle_call( disable_rebuild_all, _From, State = #s{ auto_rebuild_all = true } ) ->
	?log_info([?MODULE, disable_rebuild_all]),
	{reply, ok, State #s{ auto_rebuild_all = false }};

handle_call( enable_rebuild_all, _From, State = #s{ auto_rebuild_all = true } ) ->
	{reply, ok, State};
handle_call( enable_rebuild_all, From, State = #s{ auto_rebuild_all = false } ) ->
	?log_info([?MODULE, enable_rebuild_all]),
	handle_call( rebuild_all, From, State #s{ auto_rebuild_all = true } );

handle_call( {hook_handlers_updated, HookID}, From, State = #s{ auto_rebuild_all = AutoRebuildAll } ) ->
	?log_info([?MODULE, hook_handlers_updated, {hook_id, HookID}]),
	case AutoRebuildAll of
		false -> {reply, ok, State};
		true -> handle_call( rebuild_all, From, State )
	end;

handle_call( rebuild_all, _From, State = #s{} ) ->
	?log_info([?MODULE, rebuild_all, start]),
	{TimeConsumed, ReturnValue} =
		timer:tc( fun() ->
			AllHooks = jd_hooks_ets:handlers_all(),
			AllHookIDs = [ HookID || {HookID, _} <- AllHooks ],
			HookDispFunc = jd_hooks_forms:hook_dispatcher_function( AllHookIDs ),
			HookHandlerFuncs = [
					jd_hooks_forms:hook_function_form( HookID, HookHandlers )
					|| {HookID, HookHandlers} <- AllHooks 
				],
			ModuleForms = [
					{attribute, 0, module, jd_hooks_hook_runner_beam},
					{attribute, 0, export, [{run, 3}]},
					% {attribute, 0, export, [
					% 		{HookID, 2}
					% 		|| HookID <- AllHookIDs
					% 	]},
					HookDispFunc
					| HookHandlerFuncs
				],
			case compile:forms( ModuleForms ) of
				{ok, jd_hooks_hook_runner_beam, ModuleBinary} ->
					{module, jd_hooks_hook_runner_beam} = code:load_binary(
						jd_hooks_hook_runner_beam, 
						binary_to_list( iso8601:format(erlang:now()) ) ++ "/jd_hooks_hook_runner_beam.beam", 
						ModuleBinary ),
					{reply, ok, State};
				Error ->
					{reply, {error, Error}, State}
			end
		end),
	?log_info([?MODULE, rebuild_all, complete, {time_consumed, TimeConsumed}]),
	ReturnValue;

handle_call(Request, _From, State = #s{}) -> {stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #s{}) -> {stop, {bad_arg, Request}, State}.

handle_info(Message, State = #s{}) -> {stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) -> ignore.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% %%%%%%%% %%%
%%% Internal %%%
%%% %%%%%%%% %%%
