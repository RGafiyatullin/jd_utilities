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

-module(jd_utils_config).
-compile({parse_transform, jd_config}).
-application( jd_utils ).

-spec entity_log_dir() -> string().
-option( {entity_log_dir, [
					{default, "log/entities"},
					{validate, {list_of, [ integer, {gte, 0}, {lte, 255} ]}}
				]} ).
-spec entity_log_enabled() -> boolean().
-option( {entity_log_enabled, [
					{default, false},
					{validate, boolean}
				]} ).

