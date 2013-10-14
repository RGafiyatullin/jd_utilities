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

-module(jd_utf8).
-compile({parse_transform, gin}).
-export([
		is_7bit/1,
		to_string/1,
		to_charlist/1,

		to_lower/1,
		to_upper/1,
		to_title/1
	]).

-export([ utfl_to_bin/1, bin_to_utfl/1 ]).
-export_type([uchar/0, ucharlist/0, ustring/0]).

-opaque uchar() :: integer().
-opaque ucharlist() :: [ uchar() ].
-opaque ustring() :: binary().

-spec is_7bit( S :: ustring() ) -> boolean().
-spec to_string( In :: ucharlist() | ustring() ) -> ustring().
-spec to_charlist( S :: ustring() ) -> ucharlist().
-spec to_lower( S :: ustring() ) -> ustring().
-spec to_upper( S :: ustring() ) -> ustring().
-spec to_title( S :: ustring() ) -> ustring(). 

-define(is_string( S ), is_binary( S ) ).

-spec utfl_to_bin( L :: [integer()] ) -> binary().
utfl_to_bin( L ) -> 
	iolist_to_binary(
		lists:map( fun( Ch ) -> <<Ch/utf8>> end, lists:flatten(L) )).

-spec bin_to_utfl( B :: binary() ) -> [integer()].
bin_to_utfl( B ) -> 
	bin_to_utfl( B, [] ).

bin_to_utfl( <<>>, Acc ) -> lists:reverse(Acc);
bin_to_utfl( <<Ch/utf8, Rest/binary>>, Acc ) -> bin_to_utfl( Rest, [ Ch | Acc ] ).

is_7bit( S ) when ?is_string( S ) -> unicode:bin_is_7bit( S ).

to_string( In ) ->
	case to_string_impl( In ) of
		{error, _, _} -> error( bad_utf8 );
		Out when ?is_string( Out ) -> Out
	end.
to_string_impl( L ) when is_list( L ) -> unicode:characters_to_binary( L, utf8, utf8 );
to_string_impl( S ) when ?is_string( S ) -> unicode:characters_to_binary( S, utf8, utf8  ).

to_charlist( In ) ->
	case to_charlist_impl( In ) of
		{error, _, _} -> error( bad_utf8 );
		Out when is_list( Out ) -> Out
	end.
to_charlist_impl( S ) when ?is_string( S ) -> unicode:characters_to_list( S, utf8  );
to_charlist_impl( L ) when is_list( L ) -> unicode:characters_to_list( L, utf8  ).

to_lower( S ) when ?is_string( S ) -> unistring:to_lower( S ).
to_upper( S ) when ?is_string( S ) -> unistring:to_upper( S ).
to_title( S ) when ?is_string( S ) ->
	Chars = to_charlist( S ),
	{_, Rev} = lists:foldl(
		fun
			( Blank, {_, Acc} ) when in( Blank, [ $ , $\t, $\n, $\r ] ) ->
				{true, [ Blank | Acc ]};
			( Char,  {true, Acc} ) ->
				[UChar] = unistring:to_upper( [Char] ),
				{false, [ UChar | Acc ]};
			( Char, {false, Acc} ) ->
				[LChar] = unistring:to_lower( [Char] ),
				{false, [ LChar | Acc ]}
		end,
		{true, []}, Chars),
	to_string(lists:reverse( Rev )).

%%% %%%%% %%%
%%% Tests %%%
%%% %%%%% %%%
-include_lib("eunit/include/eunit.hrl").

is_7bit_test() ->
	Input = [
			{<<"Привет">>, false}, {<<"汉语/漢語">>, false}, {<<"How ду ю do">>, false},
			{<<"Hola!">>, true}, {<<"Quite a plain phrase">>, true} 
		],
	lists:foreach(
		fun( {S, Expected} ) ->
			?assert( is_7bit( S ) == Expected )
		end, Input ).

to_string_test() ->
	Privet = <<"Привет!">>,
	Input = [
		{[ 16#041F, 16#0440, 16#0438, 16#0432, 16#0435, 16#0442, $! ], Privet},
		{[ 16#041F, 16#0440, 16#0438, <<"вет!">> ], Privet},
		{[ 16#041F, 16#0440, [ <<"ив">> ], 16#0435, 16#0442, $! ], Privet},
		{[ 16#041F, 16#0440, [ 16#0438, 16#0432 ], 16#0435, 16#0442, $! ], Privet},
		{<<"Привет!">>, Privet},
		{"Hai!", <<"Hai!">>},
		{<<"Hello!">>, <<"Hello!">>}
	],
	lists:foreach(
		fun( {AsList, AsBinary} ) ->
			?assert( to_string( AsList ) == AsBinary )
		end, Input).

to_charlist_test() ->
	Privet = [ 16#041F, 16#0440, 16#0438, 16#0432, 16#0435, 16#0442, $! ],
	Input = [
		{<<"Привет!">>, Privet},
		{[ 16#041F, 16#0440, 16#0438, 16#0432, 16#0435, 16#0442, $! ], Privet},
		{[ 16#041F, 16#0440, [ <<"ив">> ], 16#0435, 16#0442, $! ], Privet},
		{[ 16#041F, 16#0440, [ 16#0438, 16#0432 ], 16#0435, 16#0442, $! ], Privet},
		{<<"Hai!">>, "Hai!"},
		{"Hello!", "Hello!"}
	],
	lists:foreach(
		fun( {AsBinary, AsList} ) ->
			?assert( to_charlist( AsBinary ) == AsList )
		end, Input).

to_case_test() ->
	Input = [
		{
			<<"АбВгдЕ ЖзИйклМн! AbcDefg HiJklMNop!?">>, 
			<<"АБВГДЕ ЖЗИЙКЛМН! ABCDEFG HIJKLMNOP!?">>, 
			<<"абвгде жзийклмн! abcdefg hijklmnop!?">>, 
			<<"Абвгде Жзийклмн! Abcdefg Hijklmnop!?">>
		}
	],
	lists:foreach(
		fun( {Original, Upper, Lower, Title} ) ->
			?assert( to_upper( Original ) == Upper ),
			?assert( to_lower( Original ) == Lower ),
			?assert( to_title( Original ) == Title )
		end, Input).
