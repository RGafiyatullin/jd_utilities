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

-module(jd_time).
-compile({parse_transform, gin}).
-record(dt, {
		ts :: erlang:timestamp()
	}).

-export([
		now/0,
		ts2dt/1,
		ts2dt/2,
		dt2ts/1,
		dt_norm/1,

		to_s/1,
		to_ms/1,
		to_mus/1,

		op/2,

		to_iso8601/1
	]).
-export_type([
		dt/0
	]).

-opaque dt() :: #dt{}.

-spec now() -> dt().
-spec dt2ts( dt() ) -> erlang:timestamp().
-spec ts2dt( erlang:timestamp() ) -> dt().
-spec to_s( dt() ) -> integer().
-spec to_ms( dt() ) -> integer().


-type dt_diff_direction() :: later | ago.
-type dt_diff_unit() :: mus | ms | s | m | h | d | w.
-type dt_diff() ::
		{ N :: integer(), OfWhat :: dt_diff_unit(), Direction :: dt_diff_direction() }
	|	{ [ {N :: integer(), OfWhat :: dt_diff_unit()} ], Direction :: dt_diff_direction() }.
-spec op( dt(), dt_diff() ) -> dt().

-spec to_iso8601( dt() ) -> jd_utf8:ustring().

now() -> 
	ErlangTS = os:timestamp(),
	ts2dt( ErlangTS ).

ts2dt( ErlangTS = {_, _, _} ) -> #dt{ ts = ErlangTS }.
ts2dt( Dt, ErlangTS ) -> Dt #dt{ ts = ErlangTS }.
dt2ts( #dt{ ts = ErlangTS } ) -> ErlangTS.

dt_norm( Dt = #dt{} ) ->
	TS = dt2ts( Dt ),
	ts2dt( Dt, ts_norm( TS, mu ) ).

to_s  ( #dt{ ts = {Mg,S,_ } } ) ->  Mg * 1000000 + S.
to_ms ( #dt{ ts = {Mg,S,Mu} } ) -> (Mg * 1000000 + S) * 1000 + ( Mu div 1000 ).
to_mus( #dt{ ts = {Mg,S,Mu} } ) -> (Mg * 1000000 + S) * 1000000 + Mu.

op( Dt, { Shifts, Direction } ) ->
	dt_norm(
		lists:foldl(
			fun( {ShiftBy, ShiftUnit}, DtAcc ) ->
				op_no_norm( DtAcc, { ShiftBy, ShiftUnit, Direction } )
			end,
			Dt, Shifts) );
op( Dt, { N, Unit, Direction } ) ->
	dt_norm(
		op_no_norm( Dt, { N, Unit, Direction } ) ).

to_iso8601( Dt = #dt{} ) ->
	TS = dt2ts( Dt ),
	iso8601:format( TS ).


%%% %%%%%%%% %%%
%%% Internal %%%
%%% %%%%%%%% %%%

ts_norm( {M, S, Mu}, mu ) ->
	case { Mu < 0, Mu div 1000000, Mu rem 1000000 } of
		{ false, 0, Mu } -> ts_norm( {M, S, Mu}, s );
		{ false, PlusS, MuRem } -> ts_norm( {M, S + PlusS, MuRem}, s );
		{ true, MinusS, 0 } -> ts_norm( {M, S - MinusS, 0}, s );
		{ true, MinusS, MinusMu } -> ts_norm( {M, S + MinusS - 1, 1000000 + MinusMu}, s )
	end;
ts_norm( {M, S, Mu}, s ) ->
	case { S < 0, S div 1000000, S rem 1000000 } of
		{ false, 0, S } -> { M, S, Mu };
		{ false, PlusM, SRem } -> { M + PlusM, SRem, Mu };
		{ true, MinusM, 0 } -> { M - MinusM, 0, Mu };
		{ true, MinusM, MinusS } -> { M + MinusM - 1, 1000000 + MinusS, Mu }
	end.

op_no_norm( Dt, { N, Unit, Direction } ) when in( Unit, [ mus, ms ] ) ->
	Mus = unit_convert( mus, N, Unit ),
	{ M, S, OldMu } = dt2ts( Dt ),
	ts2dt( Dt, { M, S, op_dir( Direction, OldMu, Mus )} );

op_no_norm( Dt, { N, Unit, Direction } ) when in( Unit, [ s, m, h, d, w ] ) ->
	Ss = unit_convert( s, N, Unit ),
	{ M, OldS, Mu } = dt2ts( Dt ),
	ts2dt( Dt, { M, op_dir( Direction, OldS, Ss ), Mu } ).

unit_convert( mus, N, mus ) -> N;
unit_convert( mus, N, ms ) -> N * 1000;

unit_convert( s, N, s ) -> N;
unit_convert( s, N, m ) -> N * 60;
unit_convert( s, N, h ) -> N * 60 * 60;
unit_convert( s, N, d ) -> N * 60 * 60 * 24;
unit_convert( s, N, w ) -> N * 60 * 60 * 24 * 7.

op_dir( later, N, By ) -> N + By;
op_dir( ago  , N, By ) -> N - By.


%%% %%%%% %%%
%%% Tests %%%
%%% %%%%% %%%
-include_lib("eunit/include/eunit.hrl").

ts_norm( TS ) -> ts_norm( TS, mu ).
ts_norm_test() ->
	?assert( ts_norm({0, 0, 0}) == {0, 0, 0} ),
	?assert( ts_norm({1, 1, 1000001}) == {1, 2, 1} ),
	?assert( ts_norm({1, 1, -1}) == {1, 0, 999999} ),
	?assert( ts_norm({1, 1, -1000001}) == {0, 999999, 999999} ).


ts_op_test() ->
	Now = ?MODULE:now(),
	NowMu = to_mus( Now ),
	
	MusLater = op( Now, {1, mus, later} ),
	MusAgo = op( Now, {1, mus, ago} ),
	?assert( NowMu - to_mus( MusLater ) == -1 ),
	?assert( NowMu - to_mus( MusAgo ) == 1 ),

	MsLater = op( Now, {1, ms, later} ),
	MsAgo = op( Now, {1, ms, ago} ),
	?assert( NowMu - to_mus( MsLater ) == -1000 ),
	?assert( NowMu - to_mus( MsAgo ) == 1000 ),

	SecondLater = op( Now, {1, s, later} ),
	SecondAgo = op( Now, {1, s, ago} ),
	?assert( NowMu - to_mus( SecondLater ) == -1000000 ),
	?assert( NowMu - to_mus( SecondAgo ) == 1000000 ),
	
	MinuteLater = op( Now, {1, m, later} ),
	MinuteAgo = op( Now, {1, m, ago} ),
	?assert( NowMu - to_mus( MinuteLater ) == -60000000 ),
	?assert( NowMu - to_mus( MinuteAgo ) == 60000000 ),

	HourLater = op( Now, {1, h, later} ),
	HourAgo = op( Now, {1, h, ago} ),
	?assert( NowMu - to_mus( HourLater ) == -3600000000 ),
	?assert( NowMu - to_mus( HourAgo ) == 3600000000 ),


	DayLater = op( Now, {1, d, later} ),
	DayAgo = op( Now, {1, d, ago} ),
	?assert( NowMu - to_mus( DayLater ) == -86400000000 ),
	?assert( NowMu - to_mus( DayAgo ) == 86400000000 ),

	WeekLater = op( Now, {1, w, later} ),
	WeekAgo = op( Now, {1, w, ago} ),
	?assert( NowMu - to_mus( WeekLater ) == -604800000000 ),
	?assert( NowMu - to_mus( WeekAgo ) == 604800000000 ).

ts_op_multi_test() ->
	Now = ?MODULE:now(),
	NowMu = to_mus( Now ),

	Shifts = [ {1, w}, {1, d}, {1, h}, {1, m}, {1, s}, {1, ms}, {1, mus} ],
	ShiftsMu = 1 + 1000 + 1000000 + 60000000 + 3600000000 + 86400000000 + 604800000000,

	Later = op( Now, { Shifts, later } ),
	LaterMu = to_mus( Later ),
	Ago = op( Now, { Shifts, ago } ),
	AgoMu = to_mus( Ago ),

	?assert( NowMu - LaterMu == - ShiftsMu ),
	?assert( NowMu - AgoMu == ShiftsMu ).



