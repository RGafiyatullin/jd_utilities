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

-module(jd_entity_log).
-export([
		start_link/2,
		get_pid/1,
		message/2
	]).

-define(key(I), {n, l, {?MODULE, I} }).

-include("logging.hrl").

-define(timeout, 5000).
-record(s, {
		quit = false :: boolean(),
		entity_id :: term(),
		dev = undefined :: undefined | file:io_device(),
		pids = sets:new() :: set(),
		state = flush :: flush | idle
	}).

-spec get_pid( ID :: term() ) -> pid().
get_pid( ID ) ->
	case gproc:lookup_pids( ?key(ID) ) of
		[] ->
			supervisor:start_child(jd_entity_log_sup, [ ID, self() ]),
			{P, _} = gproc:await( ?key( ID ) ),
			P;
		[ P ] -> P
	end.

-spec message( EntityID :: term(), Report :: term()) -> ok.
message( EntityID, Report ) ->
	get_pid( EntityID ) ! {message, self(), Report}, ok.


-spec start_link( EntityID :: term(), FirstPid :: pid() ) -> pid().
start_link( ID, FirstPid ) ->
	proc_lib:spawn_link( fun() ->
			init( ID, FirstPid )
		end ).

init( ID, FirstPid ) ->
	normal = case catch gproc:reg( ?key(ID) ) of
		true ->
			receive_loop( 
				remember_pid( 
					FirstPid, 
					#s{
						entity_id = ID,
						state = flush
					}) 
				),
			normal;
		_ ->
			normal
	end,
	ok.


receive_loop( #s{ quit = true } ) -> normal;
receive_loop( S = #s{
		state = flush
	} ) ->
	receive
		{message, Pid, Report} ->
			% ?log_debug(["Received a message"]),
			S_Written = write_report( Report, S #s{} ),
			S_PidRemembered = remember_pid( Pid, S_Written ),
			receive_loop( S_PidRemembered )
	after 0 ->
		case sets:size( S #s.pids ) of
			0 -> normal;
			_ -> receive_loop( S #s{ state = idle } )
		end
	end;
receive_loop( S = #s{
		state = idle
	} ) ->
	receive
		{message, Pid, Report} ->
			% ?log_debug(["Received a message"]),
			S_Written = write_report( Report, S #s{} ),
			S_PidRemembered = remember_pid( Pid, S_Written ),
			receive_loop( S_PidRemembered #s{ state = flush } );
		{'DOWN', _, process, Pid, _} ->
			% ?log_debug(["Received a DOWN"]),
			receive_loop( S #s{ state = flush, pids = sets:del_element( Pid, S #s.pids ) } )
	after ?timeout ->
		% ?log_debug(["Timeout. Quitting"]),
		normal
	end.


write_report( Report, S = #s{ entity_id = ID, dev = undefined } ) ->
	Filename = filename:append( 
			jd_utils_config:entity_log_dir(),
			file_for_id( ID ) ++ ".log"
		),
	% ?log_debug([ "About to open", {file, Filename} ]),
	case file:open( Filename, [append, {encoding, utf8}] ) of
		{ok, Dev} ->
			write_report( Report, S #s{
					dev = Dev
				} );
		_Err ->
			% ?log_debug(["Whoops", {err, Err}]),
			#s{ quit = true }
	end;

write_report( Report, S = #s{ dev = Dev } ) ->
	io:format( Dev, "~s~n", [ fmt_report( Report, fmt_ut(calendar:universal_time()) )] ),
	S.

remember_pid( Pid, S = #s{ pids = Pids } ) ->
	case sets:is_element( Pid, Pids ) of
		false ->
			erlang:monitor( process, Pid ),
			S #s{ pids = sets:add_element( Pid, Pids ) };
		true -> S
	end.

file_for_id( B ) when is_binary( B ) -> binary_to_list( B );
file_for_id( L ) when is_list( L ) -> lists:flatten(L);
file_for_id( A ) when is_atom( A ) -> atom_to_list(A);
file_for_id( N ) when is_integer( N ) -> integer_to_list(N);
file_for_id( F ) when is_float( F ) -> float_to_list(F); 
file_for_id( T ) when is_tuple( T ) ->
	case lists:reverse( lists:foldl(
		fun( E, Rev ) ->
			[ file_for_id(E), $- | Rev ]
		end,
		[],
		erlang:tuple_to_list( T )))
	of
		[] -> "UNIT";
		[ $- | FN ] -> FN
	end;
file_for_id( T ) ->
	[ $t,$e,$r,$m | integer_to_list( erlang:crc32( term_to_binary( T ) ) ) ].

two_digits(Int) ->
	List = integer_to_list(Int),
	case List of
		[ _ ] -> [ $0 | List ];
		[ _, _ ] -> List
	end.

fmt_date( {Y, M, D} ) ->
	[ integer_to_list(Y), $-, two_digits(M), $-, two_digits(D) ].

fmt_time( {H, M, S} ) ->
	[ two_digits(H), $:, two_digits(M), $:, two_digits(S), $\  ].

fmt_ut( { D, T } ) ->
	[ fmt_date( D ), $ , fmt_time( T ) ].

fmt_report( Report, Offset ) ->
	fmt_report( Report, [], Offset ).

fmt_report_to_s( T ) -> io_lib:format("~p", [T]).

fmt_report( [], Acc, _Offset ) -> Acc;

fmt_report( [ { K, V } | Report ], Acc, Offset ) ->
	fmt_report( Report,
				[ Acc, Offset, fmt_report_to_s(K), $:, $ , V ],
				wide_offset() );
fmt_report( [ V | Report ], Acc, Offset ) when is_list(V) ->
	fmt_report( Report, 
		[ Acc, Offset,
			( try iolist_to_binary(V)
			catch error:badarg -> io_lib:format("~p", [ V ]) end ) ],
			wide_offset() );
fmt_report( [ T | Report ], Acc, Offset ) ->
	fmt_report( Report,
		[ Acc, Offset, io_lib:format("~p", [ T ]) ],
		wide_offset()).


wide_offset() -> <<"                   ">>.
