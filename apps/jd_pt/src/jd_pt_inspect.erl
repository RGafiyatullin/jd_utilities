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

-module(jd_pt_inspect).
-compile({parse_transform, gin}).
-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-export([ inspect/3 ]).

-include("pt.hrl").

-type error_m( SomeT ) :: {ok, Something :: SomeT} | {error, Reason :: term()}.
-type ast() :: term().
-type path() :: [ ast() ].

-type mod_state() :: term().
-type mod_result() :: term().

-callback init( Args :: term() ) -> error_m( mod_state() ).
-callback pre_inspect( path(), ast(), mod_state() ) -> { update | break, ast(), mod_state() } | { erase, mod_state() }.
-callback post_inspect( path(), ast(), mod_state() ) -> { update, ast(), mod_state() } | { erase, mod_state() }.
-callback finalize( State :: term() ) -> error_m( mod_result() ).

-spec inspect( [ast()], module(), term() ) -> error_m( {ast(), mod_result()} ).
inspect( ASTNodes, Mod, Args ) ->
	do([ error_m ||
		SInitial <- Mod:init( Args ),
		{ASTNodesModified, StateModified} <- error_m:return(do_inspect_children_fold( ASTNodes, [], Mod, SInitial )),
		ModResult <- Mod:finalize( StateModified ),
		return( {ASTNodesModified, ModResult} )
		]).

-spec do_inspect( ast(), path(), module(), term() ) -> { ast() | erase, term() }.
do_inspect( undefined, _, _, ModS0 ) -> {undefined, ModS0};
do_inspect( Node0, Path, Mod, ModS0 ) ->
	case Mod:pre_inspect(path_push(Node0, Path), Node0, ModS0) of
		{update, Node1, ModS1} ->
			{Node2, ModS2} = do_inspect_children(
								Node1, path_push( Node1, Path ), Mod, ModS1 ),
			do_post_inspect(Node2, Path, Mod, ModS2 );
		{break, Node1, ModS1} -> {Node1, ModS1};
		{erase, ModS1} -> {erase, ModS1}
	end.

do_post_inspect( Node0, Path, Mod, ModS0 ) ->
	case Mod:post_inspect(path_push(Node0, Path), Node0, ModS0 ) of
		{update, Node1, ModS1} -> {Node1, ModS1};
		{erase, ModS1} -> {erase, ModS1}
	end.

% do_inspect_children( Node = #ast_mod_decl{}, _, _, ModS ) -> {Node, ModS};
% do_inspect_children( Node = #ast_mod_export{}, _, _, ModS ) -> {Node, ModS};
% do_inspect_children( Node = #ast_mod_import{}, _, _, ModS ) -> {Node, ModS};
% do_inspect_children( Node = #ast_mod_compile{}, _, _, ModS ) -> {Node, ModS};

do_inspect_children( Node = #ast_mod_attr{ value = ValueExpr0 }, Path, Mod, ModS0 ) ->
	case do_inspect( ValueExpr0, Path, Mod, ModS0 ) of
		{erase, ModS1} -> {erase, ModS1};
		{ValueExpr1, ModS1} ->
			{Node #ast_mod_attr{ value = ValueExpr1 }, ModS1}
	end;

do_inspect_children( Node = #ast_record_field{ value = ValueExpr0 }, Path, Mod, ModS0 ) ->
	case do_inspect( ValueExpr0, Path, Mod, ModS0 ) of
		{erase, ModS1} -> {erase, ModS1};
		{ValueExpr1, ModS1} ->
			{Node #ast_record_field{ value = ValueExpr1 }, ModS1}
	end;

do_inspect_children( Node = #ast_def_record{ fields = Fields0 }, Path, Mod, ModS0 ) ->
	{ Fields1, ModS1 } = do_inspect_children_fold( Fields0, Path, Mod, ModS0 ),
	{Node #ast_def_record{ fields = Fields1 }, ModS1};

do_inspect_children( Node = #ast_def_func{ clauses = Clauses0 }, Path, Mod, ModS0 ) ->
	{Clauses1, ModS1} = do_inspect_children_fold( Clauses0, Path, Mod, ModS0 ),
	{Node #ast_def_func{ clauses = Clauses1 }, ModS1};

do_inspect_children( Node = #ast_tuple{ elements = Elements0 }, Path, Mod, ModS0 ) ->
	{Elements1, ModS1} = do_inspect_children_fold( Elements0, Path, Mod, ModS0 ),
	{Node #ast_tuple{ elements = Elements1 }, ModS1};

do_inspect_children( Node = #ast_cons{ head = H0, tail = T0 }, Path, Mod, ModS0 ) ->
	{H1, ModS1} = do_inspect( H0, Path, Mod, ModS0 ),
	{T1, ModS2} = do_inspect( T0, Path, Mod, ModS1 ),
	{ Node #ast_cons{ head = H1, tail = T1 }, ModS2 };

do_inspect_children( Node = #ast_bin_op{ left = L0, right = R0 }, Path, Mod, ModS0 ) ->
	{L1, ModS1} = do_inspect( L0, Path, Mod, ModS0 ),
	{R1, ModS2} = do_inspect( R0, Path, Mod, ModS1 ),
	{ Node #ast_bin_op{ left = L1, right = R1 }, ModS2 };

do_inspect_children( Node = #ast_un_op{ arg = A0 }, Path, Mod, ModS0 ) ->
	{A1, ModS1} = do_inspect( A0, Path, Mod, ModS0 ),
	{ Node #ast_un_op{ arg = A1 }, ModS1 };

do_inspect_children( Node = #ast_pat_match{ left = L0, right = R0 }, Path, Mod, ModS0 ) ->
	{L1, ModS1} = do_inspect( L0, Path, Mod, ModS0 ),
	{R1, ModS2} = do_inspect( R0, Path, Mod, ModS1 ),
	{ Node #ast_pat_match{ left = L1, right = R1 }, ModS2 };

do_inspect_children( Node = #ast_expr_catch{ expr = E0 }, Path, Mod, ModS0 ) ->
	{E1, ModS1} = do_inspect( E0, Path, Mod, ModS0 ),
	{ Node #ast_expr_catch{ expr = E1 }, ModS1 };

do_inspect_children( Node = #ast_expr_call{ func = F0, args = As0 }, Path, Mod, ModS0 ) ->
	{F1, ModS1} = do_inspect( F0, Path, Mod, ModS0 ),
	{As1, ModS2} = do_inspect_children_fold( As0, Path, Mod, ModS1 ),
	{ Node #ast_expr_call{ func = F1, args = As1 }, ModS2 };

do_inspect_children( Node = #ast_expr_remote{ module = M0, func = F0 }, Path, Mod, ModS0 ) ->
	{M1, ModS1} = do_inspect( M0, Path, Mod, ModS0 ),
	{F1, ModS2} = do_inspect( F0, Path, Mod, ModS1 ),
	{ Node #ast_expr_remote{ module = M1, func = F1 }, ModS2 };

do_inspect_children( Node = #ast_expr_record{ record = R0, fields = Fs0 }, Path, Mod, ModS0 ) ->
	{R1, ModS1} = do_inspect( R0, Path, Mod, ModS0 ),
	{Fs1, ModS2} = do_inspect_children_fold( Fs0, Path, Mod, ModS1 ),
	{Node #ast_expr_record{ record = R1, fields = Fs1 }, ModS2};

do_inspect_children( Node = #ast_expr_record_field{ record = R0 }, Path, Mod, ModS0 ) ->
	{R1, ModS1} = do_inspect( R0, Path, Mod, ModS0 ),
	{Node #ast_expr_record_field{ record = R1 }, ModS1};

do_inspect_children( Node = #ast_expr_binary{ elements = Es0 }, Path, Mod, ModS0 ) ->
	{Es1, ModS1} = do_inspect_children_fold( Es0, Path, Mod, ModS0 ),
	{Node #ast_expr_binary{ elements = Es1 }, ModS1};

do_inspect_children( Node = #ast_expr_binary_element{ value = V0, size = S0, type = T0 }, Path, Mod, ModS0 ) ->
	{V1, ModS1} = do_inspect( V0, Path, Mod, ModS0 ),
	{S1, ModS2} = do_inspect( S0, Path, Mod, ModS1 ),
	{T1, ModS3} = do_inspect( T0, Path, Mod, ModS2 ),
	{Node #ast_expr_binary_element{ value = V1, size = S1, type = T1 }, ModS3 };

do_inspect_children( Node = #ast_clause{ patterns = Ps0, guards = Gs0, body = Bs0 }, Path, Mod, ModS0 ) ->
	{Ps1, ModS1} = do_inspect_children_fold( Ps0, Path, Mod, ModS0 ),
	{Gs1, ModS2} = {Gs0, ModS1}, % TODO: do something with guards here
	{Bs1, ModS3} = do_inspect_children_fold( Bs0, Path, Mod, ModS2 ),
	{Node #ast_clause{ patterns = Ps1, guards = Gs1, body = Bs1 }, ModS3};

do_inspect_children( Node = #ast_expr_case{ expr = E0, clauses = Cs0 }, Path, Mod, ModS0 ) ->
	{E1, ModS1} = do_inspect( E0, Path, Mod, ModS0 ),
	{Cs1, ModS2} = do_inspect_children_fold( Cs0, Path, Mod, ModS1 ),
	{Node #ast_expr_case{ expr = E1, clauses = Cs1 }, ModS2};

do_inspect_children( Node = #ast_expr_if{ clauses = Cs0 }, Path, Mod, ModS0 ) ->
	{Cs1, ModS1} = do_inspect_children_fold( Cs0, Path, Mod, ModS0 ),
	{Node #ast_expr_if{ clauses = Cs1 }, ModS1};

do_inspect_children( Node = #ast_expr_remote_fun_ref{ module = M0, func = F0, arity = A0 }, Path, Mod, ModS0 ) ->
	{M1, ModS1} = do_inspect( M0, Path, Mod, ModS0 ),
	{F1, ModS2} = do_inspect( F0, Path, Mod, ModS1 ),
	{A1, ModS3} = do_inspect( A0, Path, Mod, ModS2 ),
	{ Node #ast_expr_remote_fun_ref{
			module = M1,
			func = F1,
			arity = A1
		}, ModS3 };

do_inspect_children( Node = #ast_expr_lc{ expr = Expr0, quals = Quals0 }, Path, Mod, ModS0 ) ->
	{Expr1, ModS1} = do_inspect( Expr0, Path, Mod, ModS0 ),
	{Quals1, ModS2} = do_inspect_children_fold( Quals0, Path, Mod, ModS1 ),
	{ Node #ast_expr_lc{ expr = Expr1, quals = Quals1 }, ModS2 };

do_inspect_children( Node = #ast_expr_lc_generate{ each = Each0, 'of' = Of0 }, Path, Mod, ModS0 ) ->
	{Each1, ModS1} = do_inspect( Each0, Path, Mod, ModS0 ),
	{Of1, ModS2} = do_inspect( Of0, Path, Mod, ModS1 ),
	{ Node #ast_expr_lc_generate{ each = Each1, 'of' = Of1 }, ModS2 };

do_inspect_children( Node = #ast_expr_try_catch{ exprs = Es0, cases = Cases0, catches = Catches0, afters = As0 }, Path, Mod, ModS0 ) ->
	{Es1, ModS1} = do_inspect_children_fold( Es0, Path, Mod, ModS0 ),
	{Cases1, ModS2} = do_inspect_children_fold( Cases0, Path, Mod, ModS1 ),
	{Catches1, ModS3} = do_inspect_children_fold( Catches0, Path, Mod, ModS2 ),
	{As1, ModS4} = do_inspect_children_fold( As0, Path, Mod, ModS3 ),
	{Node #ast_expr_try_catch{
			exprs = Es1,
			cases = Cases1,
			catches = Catches1,
			afters = As1
		}, ModS4};

do_inspect_children( Node = #ast_expr_fun_def{ clauses = Cs0 }, Path, Mod, ModS0 ) ->
	{ Cs1, ModS1 } = do_inspect_children_fold( Cs0, Path, Mod, ModS0 ),
	{ Node #ast_expr_fun_def{ clauses = Cs1 }, ModS1 };

do_inspect_children( UnknownNode, _, _, ModS ) -> {UnknownNode, ModS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_inspect_children_fold( Children0, Path, Mod, ModS0 ) ->
	{RevChildren1, ModS1} = lists:foldl(
		fun( Child0, {Acc, ModSIn} ) ->
			case do_inspect( Child0, Path, Mod, ModSIn ) of
				{erase, ModSOut} -> {Acc, ModSOut};
				{Child1, ModSOut} -> {[Child1 | Acc], ModSOut}
			end
		end, {[], ModS0}, Children0 ),
	{lists:reverse(RevChildren1), ModS1}.

path_push( Node, Path ) -> [ Node | Path ].



