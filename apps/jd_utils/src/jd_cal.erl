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

-module(jd_cal).
-export([
		get_timestamp/0,
		get_timestamp_microseconds/0,
		get_timestamp_microseconds/1,

		ts/1, ts/2,
		utc/1, utc/2,

		s/1, s/2,

		utc_to_binary/1,

		units_to_seconds/2
	]).

-spec get_timestamp() -> pos_integer().
get_timestamp() ->
	{Mega, Sec, _Micro} = erlang:now(),
	(Mega * 1000000 + Sec). % * 1000000 + Micro.

get_timestamp_microseconds( _NowFunction = {M, F} ) ->
	{Mega, Sec, Micro} = M:F(),
	(Mega * 1000000 + Sec) * 1000000 + Micro.

get_timestamp_microseconds() ->
	{Mega, Sec, Micro} = erlang:now(),
	(Mega * 1000000 + Sec) * 1000000 + Micro.

units_to_seconds( s, S ) -> S;
units_to_seconds( m, M ) -> M * 60;
units_to_seconds( h, H ) -> H * units_to_seconds( m, 60 );
units_to_seconds( d, D ) -> D * units_to_seconds( h, 24 );
units_to_seconds( w, W ) -> W * units_to_seconds( d, 7 ).

ts( now, Base ) -> Base;
ts( {Count, Unit, ago}, Base ) ->
	{Meg, S, Mil} = Base,
	{Meg, S - units_to_seconds( Unit, Count ), Mil};
ts( {Count, Unit, later}, Base ) ->
	{Meg, S, Mil} = Base,
	{Meg, S + units_to_seconds( Unit, Count ), Mil}.

ts( When ) -> ts( When, erlang:now() ).

utc( When ) -> calendar:now_to_universal_time( ts(When) ).
utc( When, Base ) -> calendar:now_to_universal_time( ts(When, Base) ).

s( When ) -> calendar:datetime_to_gregorian_seconds( utc( When ) ).
s( When, Base ) -> calendar:datetime_to_gregorian_seconds( utc( When, Base ) ).

utc_to_binary( { {Year,Month,Day}, {Hour,Minute,Second} } ) ->
	iolist_to_binary([
			integer_to_list(Year), $-, integer_to_list(Month), $-, integer_to_list(Day), $ ,
			integer_to_list(Hour), $:, integer_to_list(Minute), $:, integer_to_list(Second)
		]).