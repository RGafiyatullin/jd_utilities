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

-module(jd_pt_parse_form).
-export([
		parse_form/1
	]).

-include("pt.hrl").

parse_form( undefined ) -> undefined;

parse_form( {attribute, Line, module, Module} ) -> #ast_mod_decl{ line = Line, module = Module };
parse_form( {attribute, Line, export, Funcs} ) -> #ast_mod_export{ line = Line, funcs = Funcs };
parse_form( {attribute, Line, import, {Mod, Funcs}} ) -> #ast_mod_import{ line = Line, module = Mod, funcs = Funcs };
parse_form( {attribute, Line, record, {Tag, Fields}} ) -> #ast_def_record{ line = Line, tag = Tag, fields = [ parse_form( DRF ) || DRF <- Fields ] };
parse_form( {attribute, Line, compile, Opts} ) -> #ast_mod_compile{ line = Line, opts = Opts };
parse_form( {attribute, Line, AttrName, AttrValue} ) -> #ast_mod_attr{ line = Line, name = AttrName, value = parse_form( AttrValue ) };
parse_form( {function, Line, FName, FArity, FClauses} ) -> #ast_def_func{ line = Line, name = FName, arity = FArity, clauses = [ parse_form(FC) || FC <- FClauses ] };

parse_form( {eof, Line} ) -> #ast_eof{ line = Line };
parse_form( {error, Err} ) -> #ast_error{ what = Err };
parse_form( {warning, Warn} ) -> #ast_warning{ what = Warn };

parse_form( {integer, Line, Value} ) -> #ast_lit_int{ line = Line, value = Value };
parse_form( {float, Line, Value} ) -> #ast_lit_float{ line = Line, value = Value };
parse_form( {string, Line, Value} ) -> #ast_lit_string{ line = Line, value = Value };
parse_form( {atom, Line, Value} ) -> #ast_lit_atom{ line = Line, value = Value };

parse_form( {var, Line, Name} ) -> #ast_var{ line = Line, name = Name };
parse_form( {tuple, Line, Elems} ) -> #ast_tuple{ line = Line, elements = [ parse_form( E ) || E <- Elems ] };

parse_form( {nil, Line} ) -> #ast_nil{ line = Line };
parse_form( {cons, Line, Head, Tail} ) -> #ast_cons{ line = Line, head = parse_form( Head ), tail = parse_form( Tail ) };

parse_form( {op, Line, Name, Left, Right} ) -> #ast_bin_op{ line = Line, name = Name, left = parse_form( Left ), right = parse_form( Right ) };
parse_form( {op, Line, Name, Arg} ) -> #ast_un_op{ line = Line, name = Name, arg = parse_form(Arg) };

parse_form( {match, Line, Left, Right} ) -> #ast_pat_match{ line = Line, left = parse_form(Left), right = parse_form(Right) };

parse_form( {'catch', Line, Expr} ) -> #ast_expr_catch{ line = Line, expr = parse_form(Expr) };
parse_form( {call, Line, F, Args} ) -> #ast_expr_call{ line = Line, func = parse_form( F ), args = [ parse_form(A) || A <- Args ] };
parse_form( {remote, Line, Mod, Func} ) -> #ast_expr_remote{ line = Line, module = parse_form(Mod), func = parse_form(Func) };


parse_form( {clause, Line, Patterns, Guards, Body} ) -> #ast_clause{ line = Line,
															patterns = [ parse_form(P) || P <- Patterns ],
															guards = Guards,
															body = [ parse_form(B) || B <- Body ] };

parse_form( {record, Line, Record, Tag, RecordFields } ) -> (parse_form( {record, Line, Tag, RecordFields} )) 
																#ast_expr_record{ record = parse_form(Record) };
parse_form( {record, Line, Tag, RecordFields} ) -> #ast_expr_record{ line = Line,
																	record = undefined, tag = Tag,
																	fields = [ parse_form(RF) || RF <- RecordFields ] };
parse_form( {record_field, Line, Record, Tag, Field} ) -> #ast_expr_record_field{ line = Line,
																	record = Record, tag = Tag,
																	field = parse_form(Field) };

parse_form( {record_field, Line, Name} ) -> #ast_record_field{ line = Line, field = parse_form(Name) };
parse_form( {record_field, Line, Name, Default} ) -> (parse_form( {record_field, Line, Name} )) #ast_record_field{ value = parse_form( Default ) };

parse_form( {bin, Line, Elements} ) -> #ast_expr_binary{ line = Line, elements = [ parse_form(E) || E <- Elements ] };
parse_form( {bin_element, Line, Value, Size, Type} ) -> #ast_expr_binary_element{ line = Line, value = parse_form(Value), size = parse_form(Size), type = parse_form(Type) };

parse_form( {'case', Line, E, Cs} ) -> #ast_expr_case{ line = Line, expr = parse_form(E), clauses = [ parse_form(C) || C <- Cs ] };
parse_form( {'if', Line, Cs} ) -> #ast_expr_if{ line = Line, clauses = [ parse_form(C) || C <- Cs ] };

parse_form( {'fun', Line, {function, Name, Arity}} ) -> #ast_expr_local_fun_ref{ line = Line, name = Name, arity = Arity };
parse_form( {'fun', Line, {function, Mod, Func, Arity}} ) -> #ast_expr_remote_fun_ref{ line = Line, module = parse_form(Mod), func = parse_form(Func), arity = parse_form(Arity) };
parse_form( {'fun', Line, {clauses, Clauses}} ) -> #ast_expr_fun_def{ line = Line, clauses = [ parse_form(C) || C <- Clauses ] };

parse_form( {'try', Line, Exprs, Cases, Catches, Afters} ) ->
		Parsed = #ast_expr_try_catch{ line = Line,
				exprs = [parse_form(E) || E <- Exprs],
				cases = [parse_form(C) || C <- Cases],
				catches = [parse_form(C) || C <- Catches],
				afters = [parse_form(A) || A <- Afters]
			},
		% io:format("try-catch-after: ~p~n", [Parsed]),
		Parsed;

parse_form( {lc, Line, Expr, LCQuals} ) ->
		#ast_expr_lc{
				line = Line,
				expr = parse_form(Expr),
				quals = [ parse_form( LCQ ) || LCQ <- LCQuals ]
			};
parse_form( {generate, Line, Each, Of} ) ->
		#ast_expr_lc_generate{
				line = Line,
				each = parse_form(Each),
				'of' = parse_form(Of)
			};
% parse_form( default ) -> #ast_expr_default{};

parse_form( UnknownForm ) ->
	% io:format( "Unknown form: ~p~n", [UnknownForm] ),
	#ast_unknown_form{ asis = UnknownForm }.


