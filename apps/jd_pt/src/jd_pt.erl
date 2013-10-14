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

-module(jd_pt).
-export([ value/1 ]).
-export([ parse_form/1, render_form/1 ]).
-export([ inspect/3 ]).

parse_form( F ) -> jd_pt_parse_form:parse_form( F ).
render_form( AST ) -> jd_pt_render_form:render_form( AST ).
inspect( ASTNodes, Module, Args ) -> jd_pt_inspect:inspect( ASTNodes, Module, Args ).

value( V ) -> parse_form( erl_parse:abstract( V ) ).