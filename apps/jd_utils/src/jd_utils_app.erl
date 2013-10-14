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

-module(jd_utils_app).
-behaviour(application).
-export([
	start/2,
	stop/1
	]).

-spec start( atom(), term() ) -> {ok, pid()}.
start(_StartType, _Args) ->
	erlang:register(?MODULE, self()),
	jd_utils_sup:start_link().

-spec stop( term() ) -> ignore.
stop(_State) ->
	ignore.


