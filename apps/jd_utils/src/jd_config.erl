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

-module (jd_config).
-behaviour (gen_server).
-export([ parse_transform/2 ]).
-export([ start_link/1 ]).
-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
]).
-export([
		get_option/2,
		set_opt/3,
		set_opt_sync/3
	]).

-include_lib("jd_utils/include/logging.hrl").
-include_lib("jd_utils/include/gen_server_spec.hrl").
-include("jd_config_internal.hrl").

parse_transform( FormsIn, Options ) -> jd_config_pt:parse_transform( FormsIn, Options ).


start_link( Mod ) ->
	gen_server:start_link( {local, Mod}, ?MODULE, { Mod }, [] ).

get_option( Mod, OptName ) ->
	case ets:lookup( Mod, OptName ) of
		[] -> error( {?MODULE, undefined_option, Mod, OptName} );
		[ {_, OptValue} ] -> OptValue
	end.

set_opt( Mod, OptName, OptValue ) -> gen_server:cast( Mod, {set_opt, OptName, OptValue} ).
set_opt_sync( Mod, OptName, OptValue ) -> gen_server:call( Mod, {set_opt, OptName, OptValue} ).

%%% %%%%%%%%%% %%%
%%% gen_server %%%
%%% %%%%%%%%%% %%%

-record(s, {
		info :: #i{},
		mod :: atom()
	}).

init({ Mod }) ->
	Info = Mod: ?defs_callback (),
	Mod = ets:new( Mod, [named_table, set, protected] ),

	AppName = Info #i.app,
	AppEnv = application:get_all_env( AppName ),
	lists:foreach( fun( #option{ name = OptName, default = OptDefault, props = OptProps } ) ->
			case lists:keyfind( OptName, 1, AppEnv ) of
				false ->
					case OptDefault of
						false -> error( { no_default, [{section, AppName}, {option, OptName}, {config_module, Mod}] } );
						{default, DefaultValue} ->
							?log_info([?MODULE, {application, AppName}, {conf_mod, Mod}, {OptName, DefaultValue}, default_used]),
							ets:insert( Mod, {OptName, DefaultValue} )
					end;
				{OptName, OptValue} ->
					ValidationResult = option_validate( OptValue, [ OptValidator || OptValidator = {validate, _} <- OptProps ] ),
					Report = [?MODULE, {application, AppName}, {conf_mod, Mod}, {OptName, OptValue}, {validation_result, ValidationResult}],
					case ValidationResult of
						[] ->
							?log_info( Report ),
							ets:insert( Mod, {OptName, OptValue} );
						[ _ | _ ] ->
							?log_fatal( Report ),
							error( {configuration_validation_error, [ {mod, Mod}, {app, AppName}, {OptName, OptValue}, {validation_report, ValidationResult} ]} )
					end
			end
		end, Info#i.options ),
	{ok, #s{
		mod = Mod,
		info = Info
	}}.

handle_call({set_opt, OptName, OptValue}, _ReplyTo, State = #s{ mod = Mod }) ->
	true = ets:insert( Mod, {OptName, OptValue} ),
	{reply, ok, State};

handle_call(Request, _From, State = #s{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({set_opt, OptName, OptValue}, State = #s{ mod = Mod }) ->
	true = ets:insert( Mod, {OptName, OptValue} ),
	{noreply, State};

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
option_validate( Value, Validators ) -> jd_config_validate:option_validate( Value, Validators ).

