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

-module(jd_log).
-export([report/2, report_el/2, report_el/1]).

-type log_level() :: debug | info | notice | warning | error | critical | alert | emergency.

-spec report(Lvl :: log_level(), Report :: term()) -> ok.
report(Lvl, ReportNoLevel) ->
	Report = add_level(Lvl, ReportNoLevel),
	% LoggerFunc = case Lvl of
	% 			debug -> info_report;
	% 			info -> info_report;
	% 			notice -> info_report;

	% 			warning -> warning_report;

	% 			error -> error_report;
	% 			critical -> error_report;
	% 			alert -> error_report;
	% 			emergency -> error_report
	% 		end,
	report_el( Lvl, Report ),
	% error_logger:LoggerFunc( Report ).
	lager:log( Lvl, self(), print_silly_list(Report), [] ).

report_el( Report ) -> report_el( debug, Report ).
report_el( Lvl, Report ) ->
	case erlang:get( entity_id ) of
		undefined -> ok;
		EntityID -> jd_entity_log:message(EntityID, [{entity_log_marker(Lvl), print_silly_list(Report)}] )
	end.

-spec add_level( Lvl :: log_level(), Report :: term() ) -> [term()].
add_level( Lvl, Report ) ->
	[
		{log_level, Lvl}
		| Report
	].


%%% %%%%% %%%%%%%%%% %%%
%%% Lager formatting %%%
%%% %%%%% %%%%%%%%%% %%%
-define(DEFAULT_TRUNCATION, 4096).

print_silly_list(L) when is_list(L) ->
    case lager_stdlib:string_p(L) of
        true ->
            lager_trunc_io:format("~s", [L], ?DEFAULT_TRUNCATION);
        _ ->
            print_silly_list(L, [], [])
    end;
print_silly_list(L) ->
    {Str, _} = lager_trunc_io:print(L, ?DEFAULT_TRUNCATION),
    Str.

print_silly_list([], Fmt, Acc) ->
    lager_trunc_io:format(string:join(lists:reverse(Fmt), ", "),
        lists:reverse(Acc), ?DEFAULT_TRUNCATION);
print_silly_list([{K,V}|T], Fmt, Acc) ->
    print_silly_list(T, ["~p: ~p" | Fmt], [V, K | Acc]);
print_silly_list([H|T], Fmt, Acc) ->
    print_silly_list(T, ["~p" | Fmt], [H | Acc]).

entity_log_marker(debug)     -> 'd';
entity_log_marker(info)      -> 'i';
entity_log_marker(notice)    -> 'n';
entity_log_marker(warning)   -> 'w';
entity_log_marker(error)     -> 'e';
entity_log_marker(critical)  -> 'critical';
entity_log_marker(alert)     -> 'alert';
entity_log_marker(emergency) -> 'emergency'.
