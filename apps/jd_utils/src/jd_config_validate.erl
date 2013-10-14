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

-module (jd_config_validate).
-export ([ option_validate/2 ]).


option_validate( OptValue, OptValidators ) ->
	_Issues = queue:to_list( option_validate( OptValue, OptValidators, queue:new() ) ).

option_validate( _, [], Report ) -> Report;
option_validate( OptValue, [ {validate, OptValidator} | OptValidators ], ReportIn ) ->
	ReportOut =
		case option_validate_single( OptValidator, OptValue ) of
			ok -> ReportIn;
			Issue -> queue:in( {OptValidator, Issue}, ReportIn )
		end,
	option_validate( OptValue, OptValidators, ReportOut ).

option_validate_single( atom, Atom ) when is_atom( Atom ) -> ok;
option_validate_single( atom, NotAnAtom ) when not is_atom( NotAnAtom ) -> {not_an_atom, NotAnAtom};

option_validate_single( module, NotAnAtom ) when not is_atom( NotAnAtom ) -> {module_not_an_atom, NotAnAtom};
option_validate_single( module, MayBeModule ) when is_atom( MayBeModule ) ->
	case code:load_file( MayBeModule ) of
		{module, Module} when MayBeModule == Module -> ok;
		{error, Error} -> {{not_a_module, Error}, MayBeModule}
	end;

option_validate_single( {enum, Items}, Value ) when is_list(Items) ->
	case lists:any( fun( Item ) -> Item == Value end, Items ) of
		true -> ok;
		false -> {{not_in_enum, Items}, Value}
	end;

option_validate_single( {maybe, _}, undefined ) -> ok;
option_validate_single( {maybe, Validators}, Value ) when is_list(Validators) ->
	case queue:to_list( option_validate( Value, [ {validate, V} || V <- Validators], queue:new() ) ) of
		[] -> ok;
		NotEmpty = [ _ | _ ] -> NotEmpty
	end;

option_validate_single( string, List ) when is_list( List ) ->
	case queue:to_list( option_validate( List, [ {validate, {list_of, [ non_neg_integer, {lte, 255} ]}} ], queue:new() ) ) of
		[] -> ok;
		[ _ | _ ] -> {not_a_string, List}
	end;
option_validate_single( string, NotAList ) when not is_list( NotAList ) -> {not_a_list, NotAList};

option_validate_single( boolean, Bool ) when is_boolean( Bool ) -> ok;
option_validate_single( boolean, NotABool ) when not is_boolean( NotABool ) -> {not_a_boolean, NotABool};

option_validate_single( integer, Integer ) when is_integer( Integer ) -> ok;
option_validate_single( integer, NotAnInt ) when not is_integer( NotAnInt ) -> {not_an_integer, NotAnInt};

option_validate_single( positive_integer, PosInt ) when is_integer( PosInt ) andalso PosInt > 0 -> ok;
option_validate_single( positive_integer, Invalid ) -> {not_a_positive_integer, Invalid};

option_validate_single( negative_integer, NegInt ) when is_integer( NegInt ) andalso NegInt < 0 -> ok;
option_validate_single( negative_integer, Invalid ) -> {not_a_negative_integer, Invalid};

option_validate_single( non_pos_integer, NonPosInt ) when is_integer( NonPosInt ) andalso NonPosInt =< 0 -> ok;
option_validate_single( non_pos_integer, Invalid ) -> {not_a_non_positive_integer, Invalid};

option_validate_single( non_neg_integer, NonNegInt ) when is_integer( NonNegInt ) andalso NonNegInt >= 0 -> ok;
option_validate_single( non_neg_integer, Invalid ) -> {not_a_non_negative_integer, Invalid};

option_validate_single( {gt, Gt}, Number ) when is_number( Number ) andalso Number > Gt -> ok;
option_validate_single( {gt, Gt}, Number ) when is_number( Number ) andalso not (Number > Gt) -> {{not_greater_than, Gt}, Number};
option_validate_single( {gt, _}, NotANumber ) when not is_number( NotANumber ) -> {not_a_number, NotANumber};

option_validate_single( {gte, Gte}, Number ) when is_number( Number ) andalso Number >= Gte -> ok;
option_validate_single( {gte, Gte}, Number ) when is_number( Number ) andalso not (Number >= Gte) -> {{not_greater_than_or_equal, Gte}, Number};
option_validate_single( {gte, _}, NotANumber ) when not is_number( NotANumber ) -> {not_a_number, NotANumber};

option_validate_single( {lt, Lt}, Number ) when is_number( Number ) andalso Number < Lt -> ok;
option_validate_single( {lt, Lt}, Number ) when is_number( Number ) andalso not (Number < Lt) -> {{not_less_than, Lt}, Number};
option_validate_single( {lt, _}, NotANumber ) when not is_number( NotANumber ) -> {not_a_number, NotANumber};

option_validate_single( {lte, Lte}, Number ) when is_number( Number ) andalso Number =< Lte -> ok;
option_validate_single( {lte, Lte}, Number ) when is_number( Number ) andalso not ( Number =< Lte ) -> {{not_less_than_or_equal, Lte}, Number};
option_validate_single( {lte, _}, NotANumber ) when not is_number( NotANumber ) -> {not_a_number, NotANumber};

option_validate_single( binary, Binary ) when is_binary( Binary ) -> ok;
option_validate_single( binary, NotABinary ) when not is_binary( NotABinary ) -> {not_a_binary, NotABinary};

option_validate_single( {list_of, Validators}, ListOfValues ) when is_list( ListOfValues ) ->
	Report = 
		lists:foldl(
			fun( Value, Report0 ) ->
				option_validate( Value, [ {validate, V} || V <- Validators ], Report0 )
			end, queue:new(), ListOfValues ),

	case queue:to_list( Report ) of
		[] -> ok;
		NonEmptyReport = [ _ | _ ] -> NonEmptyReport
	end;
option_validate_single( {list_of, _}, NotAList ) when not is_list( NotAList ) -> {not_a_list, NotAList};

option_validate_single( {mfa, M, F, A}, Value ) when is_atom(M) andalso is_atom( F ) andalso is_list( A ) -> erlang:apply( M, F, A ++ [ Value ] );

option_validate_single( Unknown, _ ) -> {unknown_validator, Unknown}.

