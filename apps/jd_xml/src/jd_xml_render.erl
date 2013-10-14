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

-module(jd_xml_render).
-compile({parse_transform, gin}).
-export([
	render/2
	]).
-include_lib("jd_xml/include/xml_t.hrl").

-record(s, {
		parent_ns = undefined,
		attr_quote = $'
	}).

render( Xml, Opts ) ->
	render_impl( Xml, apply_opts(Opts, #s{}) ).

render_impl( <<chars, Data/binary>>, _ ) -> [ escape_entities(Data) ];
render_impl( <<cdata, Data/binary>>, _ ) -> [ <<"<![CDATA[">>, Data, <<"]]>">> ];
render_impl( Element = <<xml, NCN:NS>>, S = #s{ parent_ns = ParentNS, attr_quote = Q } ) ->
	NSAttr =
		case ParentNS == NS of
			true -> [];
			false -> [<<" xmlns=">>, Q, NS, Q]
		end,
	Tail =
		case jd_xml:ch( Element ) of
			[] -> [ $/, $> ];
			Chs = [ _ | _ ] ->
				[[ $> | [ render_impl(C, S #s{ parent_ns = NS }) || C <- Chs ] ], $<, $/, NCN, $> ]
		end,
	Attrs = jd_xml:attrs( Element ),
	[ $<, NCN, NSAttr,
		[
			[ $ , AttrName, $=, Q, escape_entities( AttrValue ), Q ]
			|| {AttrName, AttrValue} <- Attrs
		], Tail ].

%% naive yet should work
escape_entities( Bin ) ->
	lists:foldl(
		fun({What, With}, B) -> binary:replace(B, What, With, [global]) end, Bin,
		[
			{<<"&">>, <<"&amp;">>},
			{<<"'">>, <<"&apos;">>},
			{<<"\"">>, <<"&quot;">>},
			{<<"<">>, <<"&lt;">>},
			{<<">">>, <<"&gt;">>}
		]).


apply_opts([], S = #s{}) -> S;
apply_opts([ {attr_quote, Q} | Opts ], S) when in(Q, [ $\', $\" ]) -> apply_opts( Opts, S #s{ attr_quote = Q } );
apply_opts([ {parent_ns, NS} | Opts ], S) when is_binary( NS ) -> apply_opts( Opts, S #s{ parent_ns = NS } ).
