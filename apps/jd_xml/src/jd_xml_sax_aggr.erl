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

-module(jd_xml_sax_aggr).
-export([ new/0, new/1, event/2 ]).

-include("xml.hrl").
-include("sax.hrl").
-include("xml_t.hrl").

-record(s, {
		current_depth = 0,
		max_depth = 10,
		stack = []
	}).

new() -> #s{}.
new( Opts ) ->
	set_opts( Opts, new() ).

-spec event( sax_event(), #s{} ) -> {more, #s{}} | {element, jd_xml:xml()}.
event( #sax_document_start{}, S ) -> {more, S};
event( #sax_document_end{}, _S ) -> error(unexpected_document_end);
event( #sax_prefix_mapping_start{}, S) -> {more, S};
event( #sax_prefix_mapping_end{}, S ) -> {more, S};
event( #sax_element_start{ ns = NS, ncn = NCN, attrs = Attrs }, S ) ->
	Element = jd_xml:set_attrs( Attrs, <<xml, NCN:NS>> ),
	{more, push(Element, S)};
event( #sax_element_end{ ns = NS, ncn = NCN }, S0 ) ->
	case pop( S0 ) of
		empty -> error( unexpected_element_end );
		{E = <<xml, NCN:NS>>, S1} -> element_close( E, S1 )
	end;
event( #sax_chars{ data = Data }, S = #s{} ) -> element_close( <<cdata, Data/binary>>, S );
event( #sax_whitespace{}, S ) -> {more, S};
event( #sax_processing_instruction{}, S ) -> {more, S};
event( #sax_error{ description = D }, _S ) -> {error, D};
event( #sax_internal_error{ description = D }, _S ) -> {internal_error, D}.

element_close( E, #s{ stack = [] } ) -> {element, reverse_children( E )};
element_close( E, S = #s{ stack = [ Parent | Stack ] } ) ->
	{more, S #s{ stack = [ jd_xml:add_ch_front( reverse_children(E), Parent ) | Stack ] }}.

reverse_children( CData = <<cdata, _/binary>> ) -> CData;
reverse_children( Chars = <<chars, _/binary>> ) -> Chars;
reverse_children( E ) ->
	RevChs = jd_xml:ch( E ),
	jd_xml:set_chs( lists:reverse(RevChs), E ).

push( E, S = #s{ stack = Stack } ) -> S #s{ stack = [E | Stack] }.
pop( #s{ stack = [] } ) -> empty;
pop( S = #s{ stack = [ E | Stack ] } ) -> { E, S #s{stack = Stack} }.
	
set_opts( [], S ) -> S;
set_opts( [ {max_depth, D} | Opts ], S ) when is_integer( D ) andalso D > 0 -> set_opts( Opts, S #s{ max_depth = D } ).
