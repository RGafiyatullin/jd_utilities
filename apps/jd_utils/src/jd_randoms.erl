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

-module(jd_randoms).
-export([start_link/0]).
-export([init/0]).
-export([s/0, s/1]).
-export([b/0, b/1]).
-export([i/0]).

-spec start_link() -> {ok, pid()}.
start_link() ->
	P = proc_lib:spawn_link( ?MODULE, init, [] ),
	{ ok, P }.

init() ->
	true = register( ?MODULE, self() ),
	{ S1, S2, S3 } = now(),
    random:seed( S1, S2, S3 ),
    loop().

loop() ->
	receive
		{random_request, From, Ref, Max} ->
			Random = random:uniform(Max),
			From ! {random_reply, Ref, Random},
			loop();
		_ ->
			loop()
	end.

-spec s() -> string().
s() ->
	s("").

-spec s(string()) -> string().
s(Base) ->
	Base ++ integer_to_list( i() ).

-spec b() -> binary().
b() ->
	b("").

-spec b(string()) -> binary().
b(Base) ->
	list_to_binary( s(Base) ).

-spec i() -> integer().
i() ->
	Ref = erlang:make_ref(),
	?MODULE ! { random_request, self(), Ref, 65536 * 65536 },
	receive { random_reply, Ref, Random } -> Random end.
