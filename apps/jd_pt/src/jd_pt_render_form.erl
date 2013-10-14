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

-module(jd_pt_render_form).
-export([render_form/1]).
-include("pt.hrl").

render_form( undefined ) -> undefined;
render_form( #ast_mod_decl{ line = Line, module = Module } ) -> {attribute, Line, module, Module};
render_form( #ast_mod_export{ line = Line, funcs = Funcs } ) -> {attribute, Line, export, Funcs};
render_form( #ast_mod_import{ line = Line, module = Mod, funcs = Funcs } ) -> {attribute, Line, import, {Mod, Funcs}};
render_form( #ast_def_record{ line = Line, tag = Tag, fields = DRFs } ) -> {attribute, Line, record, {Tag, [ render_form(DRF) || DRF <- DRFs ]}};
render_form( #ast_mod_compile{ line = Line, opts = Opts } ) -> {attribute, Line, compile, Opts};
render_form( #ast_mod_attr{ line = Line, name = Name, value = Value } ) -> {attribute, Line, Name, render_form(Value)};
render_form( #ast_def_func{ line = Line, name = FName, arity = FArity, clauses = FClauses } ) -> 
	{function, Line, FName, FArity, [ render_form(FClause) || FClause <- FClauses ]};
render_form( #ast_eof{ line = Line } ) -> {eof, Line};
render_form( #ast_error{ what = Err } ) -> {error, Err};
render_form( #ast_warning{ what = Warn } ) -> {warning, Warn};
render_form( #ast_lit_int{ line = Line, value = Value } ) -> {integer, Line, Value};
render_form( #ast_lit_float{ line = Line, value = Value } ) -> {float, Line, Value};
render_form( #ast_lit_string{ line = Line, value = Value } ) -> {string, Line, Value};
render_form( #ast_lit_atom{ line = Line, value = Value } ) -> {atom, Line, Value};
render_form( #ast_var{ line = Line, name = Name } ) -> {var, Line, Name};
render_form( #ast_tuple{ line = Line, elements = Elems } ) -> {tuple, Line, [ render_form(E) || E <- Elems ]};
render_form( #ast_nil{ line = Line } ) -> {nil, Line};
render_form( #ast_cons{ line = Line, head = H, tail = T } ) -> {cons, Line, render_form(H), render_form(T)};
render_form( #ast_bin_op{ line = Line, name = N, left = L, right = R } ) -> {op, Line, N, render_form(L), render_form(R)};
render_form( #ast_un_op{ line = Line, name = N, arg = A } ) -> {op, Line, N, render_form(A)};
render_form( #ast_pat_match{ line = Line, left = L, right = R } ) -> {match, Line, render_form(L), render_form(R)};
render_form( #ast_expr_catch{ line = Line, expr = E } ) -> {'catch', Line, render_form(E)};
render_form( #ast_expr_call{ line = Line, func = F, args = As } ) -> {call, Line, render_form(F), [render_form(A) || A <- As ]};
render_form( #ast_expr_remote{ line = Line, module = M, func = F } ) -> {remote, Line, render_form(M), render_form(F)};
render_form( #ast_clause{ line = Line, patterns = Ps, guards = Gs, body = Bs } ) -> {clause, Line, [ render_form(P) || P <- Ps ], Gs, [render_form(B) || B <- Bs ]};
render_form( #ast_expr_record{ line = Line, record = undefined, tag = Tag, fields = Fs } ) -> {record, Line, Tag, [render_form(F) || F <- Fs]};
render_form( #ast_expr_record{ line = Line, record = Record, tag = Tag, fields = Fs } ) -> {record, Line, render_form(Record), Tag, [render_form(F) || F <- Fs]};
render_form( #ast_expr_record_field{ line = Line, record = R, tag = T, field = F } ) -> {record_field, Line, R, T, render_form(F)};
render_form( #ast_record_field{ line = Line, field = N, value = undefined } ) -> {record_field, Line, render_form(N)};
render_form( #ast_record_field{ line = Line, field = N, value = V } ) -> {record_field, Line, render_form(N), render_form(V)};
render_form( #ast_expr_binary{ line = Line, elements = Es } ) -> {bin, Line, [render_form(E) || E <- Es ]};
render_form( #ast_expr_binary_element{ line = Line, value = V, size = S, type = T } ) -> {bin_element, Line, render_form(V), render_form(S), render_form(T)};
render_form( #ast_expr_case{ line = Line, expr = E, clauses = Cs } ) -> {'case', Line, render_form(E), [render_form(C) || C <- Cs]};
render_form( #ast_expr_if{ line = Line, clauses = Cs } ) -> {'if', Line, [ render_form(C) || C <- Cs ]};
render_form( #ast_expr_local_fun_ref{ line = L, name = N, arity = A } ) -> {'fun', L, {function, N, A}};
render_form( #ast_expr_remote_fun_ref{ line = L, module = M, func = F, arity = A } ) -> {'fun', L, {function, render_form(M), render_form(F), render_form(A)}};
render_form( #ast_expr_fun_def{ line = Line, clauses = Clauses } ) ->  {'fun', Line, {clauses, [ render_form(C) || C <- Clauses ]}};
render_form( #ast_expr_try_catch{ line = L, exprs = Es, cases = Cases, catches = Catches, afters = As } ) ->
		{'try', L, [render_form(E) || E <- Es], [render_form(C) || C <- Cases ], [render_form(C) || C <- Catches ], [render_form(A) || A <- As] };
render_form( #ast_expr_lc{ line = L, expr = E, quals = Qs } ) -> {lc, L, render_form(E), [ render_form(Q) || Q <- Qs ]};
render_form( #ast_expr_lc_generate{ line = L, each = Each, 'of' = Of } ) -> {generate, L, render_form(Each), render_form(Of) };
% render_form( #ast_expr_default{} ) -> default;

render_form( #ast_unknown_form{ asis = AsIs } ) -> AsIs.

