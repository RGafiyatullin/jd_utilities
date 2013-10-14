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

-module (jd_config_pt).
-export([parse_transform/2]).

-include("jd_config_internal.hrl").

parse_transform(Forms0, _Options) ->
	Info = gather_info( Forms0, #i{} ),
	% io:format( "Info: ~p~n", [Info] ),
	Forms1 = add_start_link_function( Info, Forms0 ),
	Forms2 = add_defs_function( Info, Forms1 ),
	ModName = Info #i.mod,
	Forms3 = lists:foldl( fun( OptionDef, FormsAcc ) ->
			add_option_function( ModName, OptionDef, FormsAcc )
		end, Forms2, Info #i.options ),
	% io:format("~p:: info: ~p~n", [ ?MODULE, Info ]),
	% io:format("~p:: forms: ~p~n", [ ?MODULE, Forms3 ]),
	Forms3.

add_defs_function( Info, FormsIn ) ->
	Export = {attribute, 0, export, [{ ?defs_callback, 0}]},
	Func = { function, 0, ?defs_callback, 0, [ { clause, 0, [], [], [ erl_parse:abstract( Info ) ] } ] },
	add_export_and_func( Export, Func, FormsIn ).

add_option_function( ModName, #option{ name = OptName, line = OptLine, default = _OptDefault }, FormsIn ) ->
	Export = {attribute, 0, export, [{ OptName, 0}]},
	Func =
		{ function, OptLine, OptName, 0, [
				{ clause, OptLine, [], [], [
					{call, OptLine,
						{remote, OptLine, {atom, OptLine, jd_config},{atom, OptLine, get_option}},
						[ erl_parse:abstract( ModName ), erl_parse:abstract( OptName ) ]
					}
				] }
			] },
	add_export_and_func( Export, Func, FormsIn ).

add_start_link_function( #i{ mod = Mod }, FormsIn ) ->
	Export = {attribute, 0, export, [{start_link, 0}]},
	Func =
		{ function, 0, start_link, 0, [
				{ clause, 0, [], [], [
					{call, 0,
						{remote, 0, {atom, 0, jd_config},{atom, 0, start_link}},
						[ erl_parse:abstract( Mod ) ]
					}
				] }
			] },
	add_export_and_func( Export, Func, FormsIn ).

add_export_and_func( Export, Func = {function, _, FunctionName, FunctionArity, _}, FormsIn ) ->
	FormsNoFunc = queue:to_list(
		lists:foldl(
			fun
				( ModuleDefinition = {attribute, _, module, _}, Acc ) -> queue:in( Export, queue:in( ModuleDefinition, Acc ) );
				( Anything, Acc ) -> queue:in( Anything, Acc )
			end, queue:new(), FormsIn) ),
	case lists:filter(
		fun
			( {function, _, FN, FA, _} )
					when FN == FunctionName
					andalso FA == FunctionArity
				->
					true;
			( _ ) -> false
		end,
		FormsNoFunc )
	of
		[] -> FormsNoFunc ++ [ Func ];
		[ _ | _ ] -> FormsNoFunc
	end. 


%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%
%%% Gather info from the original source forms. %%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%
gather_info( [], S ) -> S;

gather_info( [ {attribute, _, module, ModName} | Forms ], S = #i{} ) -> gather_info( Forms, S #i{ mod = ModName } );

%% Handle forms like this: -application( jd_c2s ).
gather_info( [ {attribute, _, application, AppName} | Forms ], S = #i{ app = undefined } ) when is_atom(AppName) ->
	gather_info( Forms, S #i{ app = AppName } );
gather_info( [ {attribute, Line, application, AppName} | Forms ], S = #i{ app = undefined, errors = Es } ) when not is_atom(AppName) ->
	gather_info( Forms, S #i{ errors = [
		{application_not_atom, Line, AppName} 
		| Es ]} );
gather_info( [ {attribute, Line, application, AppName} | Forms ], S = #i{ app = Defined, errors = Es } ) when is_atom(AppName) andalso Defined /= undefined ->
	gather_info( Forms, S #i{ errors = [
		{application_not_once, Line}
		| Es ] } );


%% Handle forms like these:
%% * -option( interfaces ).
%% * -option( {s2c_ping_interval, [ {default, 120000} ]} ).
gather_info( [ {attribute, Line, option, OptionName} | Forms ], S = #i{} ) when is_atom(OptionName) -> gather_info( [ {attribute, Line, option, {OptionName, []}} | Forms ], S );
gather_info( [ {attribute, Line, option, {OptionName, OptionProperties}} | Forms ], S = #i{ options = Os, errors = Es } ) when is_atom( OptionName ) andalso is_list( OptionProperties ) ->
	case lists:keyfind( OptionName, #option.name, Os ) of
		#option{} -> gather_info( Forms, S #i{ errors = [
			{option_not_unique, Line, OptionName}
			| Es ] } );
		false ->
			gather_info( Forms, S #i{ options = [
				#option{ line = Line, name = OptionName, default = lists:keyfind( default, 1, OptionProperties ), props = OptionProperties }
				| Os ] } )
	end;

%% Pass everything else through as is
gather_info( [ _ | Forms ] , S ) -> gather_info( Forms, S ).
