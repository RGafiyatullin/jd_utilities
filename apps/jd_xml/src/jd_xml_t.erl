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

-module(jd_xml_t).
-behaviour(jd_pt_inspect).
-export([parse_transform/2]).
-export([init/1, finalize/1, pre_inspect/3, post_inspect/3]).

-include_lib("jd_pt/include/pt.hrl").

-record(pt, { inspected = 0 }).
inc( PT = #pt{ inspected = I } ) -> PT #pt{ inspected = I + 1 }.

init( {} ) -> error_m:return( #pt{} ).
finalize( #pt{ inspected = I } ) -> error_m:return( I ).


pre_inspect(
	Path,
	Bin = #ast_expr_binary{
		elements = [ #ast_expr_binary_element{ value = #ast_lit_atom{value = cdata} } | _ ]
	}, S
) ->
	print_path(Path),
	{update, process_xml_cdata( Bin ), inc(S)};
pre_inspect(
	Path,
	Bin = #ast_expr_binary{
		elements = [ #ast_expr_binary_element{ value = #ast_lit_atom{value = chars} } | _ ]
	}, S
) ->
	print_path(Path),
	Chars = process_xml_chars( Bin ),
	% io:format("Chars: ~p~n", [Chars]),
	% io:format("Chars: ~p~n", [Chars]),
	{update, Chars, inc(S)};

pre_inspect(
	Path,
	Bin = #ast_expr_binary{
		elements = [ #ast_expr_binary_element{ value = #ast_lit_atom{value = xml} } | _ ]
	}, S
) ->
	print_path(Path),
	{update, process_xml_literal( Bin ), inc(S)};

pre_inspect( Path, N, S ) ->
	print_path( Path ),
	{update, N, S}.

print_path( _Path ) ->
	% PTags = [ element(1, T) || T <- Path, is_tuple(T) ],
	% io:format("Path: ~p~n", [PTags]),
	ok.

post_inspect( _Path, N, S ) -> {update, N, S}.

parse_transform( Forms, Options ) ->
	ASTNodes0 = [ jd_pt:parse_form( F ) || F <- Forms ],
	io:format( "Applying ~p (~p) with options: ~p~n", [?MODULE, [ M || #ast_mod_decl{ module = M } <- ASTNodes0 ], Options] ),
	{ok, {ASTNodes1, _InspectResult}} = jd_pt:inspect( ASTNodes0, ?MODULE, {} ),
	lists:map(fun(AST) ->
			jd_pt:render_form(AST)
		end, ASTNodes1).

%%% %%%%%%%% %%%
%%% Internal %%%
%%% %%%%%%%% %%%
process_xml_literal(
	_XmlLiteral = #ast_expr_binary{
		line = BinLine,
		elements = [
			#ast_expr_binary_element{ value = #ast_lit_atom{value = xml} }
			| BinChildren
		]
	} ) ->
		% io:format( "XML Literal. Bin-Children: ~p~n", [_XmlLiteral] ),
		[ #ast_expr_binary_element{ value = NCN0, size = NS0 } | AttrsAndChildren ] = BinChildren,
		NCN = process_xml_literal_ncn( NCN0 ),
		NS = process_xml_literal_ns( NS0 ),

		{RevAttrs, Children} = process_xml_literal_attrs_and_children( AttrsAndChildren ),
		
		AttrsField = lists:foldl(
			fun( {AttrName, AttrValue, AttrLine}, Tail ) ->
				#ast_cons{
					line = AttrLine,
					head = #ast_tuple{ line = AttrLine, elements = [ AttrName, AttrValue ] },
					tail = Tail }
			end,
			#ast_nil{}, RevAttrs),

		RecordFields0 = [
				#ast_record_field{ field = jd_pt:value(ns), value = NS },
				#ast_record_field{ field = jd_pt:value(ncn), value = NCN }
			],
		RecordFields1 =
			case AttrsField of
				#ast_nil{} -> RecordFields0;
				_ -> [ #ast_record_field{ field = jd_pt:value(a), value = AttrsField } | RecordFields0 ]
			end,
		RecordFields2 =
			case Children of
				no_children -> RecordFields1;
				_ -> [ #ast_record_field{ field = jd_pt:value(c), value = Children } | RecordFields1 ]

			end,
		#ast_expr_record{ line = BinLine,
			tag = 'xe',
			fields = RecordFields2
			}.


process_xml_literal_ns( S ) -> process_xml_to_binary_or_value( S ).
process_xml_literal_ncn( S ) -> process_xml_to_binary_or_value( S ).
process_xml_to_binary_or_value( #ast_lit_atom{ value = V } ) -> jd_pt:value(atom_to_binary(V, latin1));
process_xml_to_binary_or_value( #ast_lit_string{ value = V } ) -> jd_pt:value(jd_utf8:to_string(V));
process_xml_to_binary_or_value( AsIs ) -> AsIs.

process_xml_literal_attrs_and_children( AttrsAndChildren ) ->
	{Attrs, Children} = lists:foldl(
		fun
			( #ast_expr_binary_element{
					line = AttrLine,
					value = #ast_lit_atom{ value = AttrName },
					size = AttrValue
				},
				{As, Chs}
			) ->
				{ [ {jd_pt:value(atom_to_binary(AttrName, latin1)), process_xml_to_binary_or_value(AttrValue), AttrLine} | As ], Chs };
			( #ast_expr_binary_element{
					line = ElementListLine,
					value = ElementListValue
				}, {As, Chs} ) ->
				{ As,
					case Chs of
						no_children -> ElementListValue;
						_ -> #ast_bin_op{ line = ElementListLine, name = '++', left = Chs, right = ElementListValue }
					end }
		end,
		{[], no_children}, AttrsAndChildren),
	{Attrs, Children}.

process_xml_cdata(
	CDataLiteral = #ast_expr_binary{
		line = BinLine,
		elements = [
			#ast_expr_binary_element{ value = #ast_lit_atom{value = cdata} }
			| BinChildren
		]
	}
) ->
	RecordFields = case BinChildren of
		[] -> [];
		[ _ | _ ] ->
			[ #ast_record_field{ line = BinLine,
				field = jd_pt:value( data ),
				value = CDataLiteral #ast_expr_binary{
					elements = lists:map(
						fun xml_chars_and_bins_elements/1, BinChildren ) }
			} ]
	end,
	#ast_expr_record{ line = BinLine,
		tag = 'xd',
		fields = RecordFields
	}.

process_xml_chars(
	CharsLiteral = #ast_expr_binary{
		line = BinLine,
		elements = [
			#ast_expr_binary_element{ value = #ast_lit_atom{ value = chars} }
			| BinChildren
		]
	}
) ->
	RecordFields = case BinChildren of
		[] -> [];
		[ _ | _ ] ->
			[ #ast_record_field{
				field = jd_pt:value( data ),
				value = CharsLiteral #ast_expr_binary{
					elements = lists:map(
						fun xml_chars_and_bins_elements/1, BinChildren ) }
			} ]
	end,
	#ast_expr_record{ line = BinLine,
		tag = 'xc',
		fields = RecordFields
	}.

xml_chars_and_bins_elements( BE = #ast_expr_binary_element{ value = V, size = #ast_unknown_form{ asis = BESize }, type = #ast_unknown_form{ asis = BEType } } ) ->
	case {V, BEType, BESize} of
		{#ast_lit_string{}, default, default} -> BE;
		_ -> BE #ast_expr_binary_element{ type = #ast_unknown_form{ asis = [binary] } }
	end.
