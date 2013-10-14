-ifndef(jd_pt_include_pt_hrl).
-define(jd_pt_include_pt_hrl, true).

-type undet_type() :: term().
% -type arity() :: non_neg_integer().
% -type module() :: atom().
-type func() :: atom().

-type line_number() :: non_neg_integer().

%% Module declaration and forms ( http://www.erlang.org/doc/apps/erts/absform.html#id80833 )
-record( ast_mod_decl,	{ line = 0, module :: module() } ).
-record( ast_mod_export,{ line = 0, funcs :: [ {func(), arity()} ]} ).
-record( ast_mod_import,{ line = 0, module :: module(), funcs :: [ {func(), arity()} ]} ).
-record( ast_mod_compile,{ line = 0, opts :: term() } ).
-record( ast_mod_attr,	{ line = 0, name :: atom(), value :: ast_expression() } ).

-record( ast_record_field, { line = 0, field :: atom(), value :: ast_expression() }).
-record( ast_def_record,{ line = 0, tag :: atom(), fields :: [ #ast_record_field{} ] } ).

-record( ast_def_func,	{ line = 0, name :: func(), arity :: arity(), clauses :: [ ast_clause() ] } ).

-record( ast_error, { what } ).
-record( ast_warning, { what } ).
-record( ast_eof, { line = 0 } ).

%% Atomic literals ( http://www.erlang.org/doc/apps/erts/absform.html#id81041 )
-record( ast_lit_int,	{ line = 0, value :: integer() } ).
-record( ast_lit_float,	{ line = 0, value :: float() } ).
-record( ast_lit_string,{ line = 0, value :: string() } ).
-record( ast_lit_atom,	{ line = 0, value :: atom() } ).


%% Patterns and Expressions
-record( ast_var,	{ line = 0, name :: atom() } ).
-record( ast_tuple,	{ line = 0, elements :: [ undet_type() ] } ).

-record( ast_nil,	{ line = 0 } ).
-record( ast_cons,	{ line = 0, head :: ast_expression(), tail :: ast_expression() } ).
-record( ast_bin_op,{ line = 0, name :: atom(), left :: ast_expression(), right :: ast_expression() } ).
-record( ast_un_op,	{ line = 0, name :: atom(), arg :: ast_expression() } ).

% -record( ast_record_index, { line = 0, tag :: atom(), field :: atom() } ).

%% Patterns ( http://www.erlang.org/doc/apps/erts/absform.html#id81103 )
-record( ast_pat_match, { line = 0, left :: undet_type(), right :: undet_type() } ).

%% Expressions ( http://www.erlang.org/doc/apps/erts/absform.html#id81317 )
-record( ast_expr_catch,{ line = 0, expr :: ast_expression() } ).
-record( ast_expr_call,	{ line = 0, func :: ast_expression(), args :: [ ast_expression() ] } ).
-record( ast_expr_remote, { line = 0, module :: ast_expression(), func :: ast_expression() } ).

-record( ast_expr_record, { line = 0, record = undefined :: ast_expression(), tag :: atom(), fields :: [ #ast_record_field{} ] } ).
-record( ast_expr_record_field, { line = 0, record :: ast_expression(), tag :: atom(), field :: undet_type() } ).

-record( ast_expr_binary, { line = 0, elements }).
-record( ast_expr_binary_element, { line = 0, value :: ast_expression(), size :: ast_expression(), type :: ast_expression() }).

-record( ast_expr_case, { line = 0, expr :: ast_expression(), clauses :: [ ast_clause() ] } ).
-record( ast_expr_if, { line = 0, clauses :: [ ast_clause() ] } ).

-record( ast_expr_remote_fun_ref, { line = 0, module :: ast_expression(), func :: ast_expression(), arity :: ast_expression() } ).
-record( ast_expr_local_fun_ref, { line = 0, name :: func(), arity :: arity() } ).
-record( ast_expr_fun_def, { line = 0, clauses :: [ast_clause()] } ).
-record( ast_expr_try_catch, { line = 0, exprs :: ast_expression(), cases :: [ ast_clause() ], catches :: [ ast_clause() ], afters :: [ ast_expression() ] } ).

-record( ast_expr_lc, { line = 0, expr :: ast_expression(), quals :: [ ast_expression() ] } ).
-record( ast_expr_lc_generate, { line = 0, each :: ast_expression(), 'of' :: ast_expression() } ).

% -record( ast_expr_default, {} ).

%% Clauses ( http://www.erlang.org/doc/apps/erts/absform.html#id81998 )
-record( ast_clause, { line = 0, patterns = [] :: [ ast_expression() ], guards = [] :: [undet_type()], body :: [ ast_expression() ] } ).

%% Guards ( http://www.erlang.org/doc/apps/erts/absform.html#id82218 )



-record( ast_unknown_form, { asis } ).

-type ast_clause() :: #ast_clause{}.
-type ast_expression() :: #ast_unknown_form{}.


-endif. % jd_pt_include_pt_hrl