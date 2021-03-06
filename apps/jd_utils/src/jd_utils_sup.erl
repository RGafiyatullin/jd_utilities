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

-module(jd_utils_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-include_lib("jd_utils/include/supervisor_spec.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
	{ok,
			{{one_for_one, 5, 30}, [
				{randoms,
					{jd_randoms, start_link, []},
					permanent, 1000, worker, [jd_randoms]},
				{config,
					{jd_utils_config, start_link, []},
					permanent, 1000, worker, [jd_utils_config]},
				{entity_log_sup,
					{jd_entity_log_sup, start_link, []},
					permanent, infinity, supervisor, [jd_entity_log_sup]}
			]}
		}.
