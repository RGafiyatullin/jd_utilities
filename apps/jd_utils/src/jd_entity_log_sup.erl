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

-module(jd_entity_log_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-include_lib("jd_utils/include/supervisor_spec.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
	{ok,
			{{simple_one_for_one, 5, 30}, [
				{entity_log,
					{jd_entity_log, start_link, []},
					temporary, 100000, worker, [jd_entity_log]}
			]}
		}.

