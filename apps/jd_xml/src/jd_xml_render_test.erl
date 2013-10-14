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

-module (jd_xml_render_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("jd_xml/include/xml_t.hrl").

render( Xml ) -> jd_xml:render( Xml ).

test_render( E, B ) ->
	R = iolist_to_binary( render( E ) ),
	io:format("~nE: ~p~nB:~p~nR:~p~n~n", [ E, B, R ]),
	?assert( B == R ).

render_cdata_test() -> test_render( <<cdata, "this is cdata">>, <<"<![CDATA[this is cdata]]>">>).
render_chars_test() -> test_render( <<chars, "these are chars">>,<<"these are chars">> ).



render_el0_test() ->
	test_render(
		<<xml, ncn:ns>>,
		<<"<ncn xmlns='ns'/>">>
	).
render_el1_test() ->
	test_render(
		<<xml, ncn0:ns0, a1:v1, a2:v2>>,
		<<"<ncn0 xmlns='ns0' a1='v1' a2='v2'/>">>
	).
	

render_el2_test() ->
	test_render(
		<<xml, ncn0:ns0, a1:v1, a2:v2, [ <<xml, ncn1:ns1, a3:v3, a4:v4>> ]>>,
		<<"<ncn0 xmlns='ns0' a1='v1' a2='v2'><ncn1 xmlns='ns1' a3='v3' a4='v4'/></ncn0>">>
	).

render_el3_test() ->
	test_render(
		<<xml, ncn0:ns0, a1:v1, a2:v2, [ <<chars, "character data">> ]>>,
		<<"<ncn0 xmlns='ns0' a1='v1' a2='v2'>character data</ncn0>">>
	).

render_el4_test() ->
	test_render(
		<<xml, ncn0:ns0, a1:v1, a2:v2, [ <<cdata, "unparsed character data">> ]>>,
		<<"<ncn0 xmlns='ns0' a1='v1' a2='v2'><![CDATA[unparsed character data]]></ncn0>">>
	).

render_el5_test() ->
	test_render(
		<<xml, ncn:ns, a:"something here">>,
		<<"<ncn xmlns='ns' a='something here'/>">>
	).

render_entities_in_attrvalues_test() ->
	test_render(
		<<xml, ncn:ns, a:"is it still a'right? Health & care">>,
		<<"<ncn xmlns='ns' a='is it still a&apos;right? Health &amp; care'/>">>
	).

render_entities_in_parsed_character_data_test() ->
	test_render(
		<<xml, ncn:ns, [<<chars, "is it still a'right? Health & care">>]>>,
		<<"<ncn xmlns='ns'>is it still a&apos;right? Health &amp; care</ncn>">>
	).

ns_inherit0_test() ->
	test_render(
		<<xml, ncn1:ns1, [<<xml, ncn2:ns1>>, <<xml, ncn3:ns3>>]>>,
		<<"<ncn1 xmlns='ns1'><ncn2/><ncn3 xmlns='ns3'/></ncn1>">>
	).

ns_inherit1_test() ->
	E = <<xml, ncn:ns>>,
	B = <<"<ncn/>">>,
	R = iolist_to_binary( jd_xml:render( E, [{parent_ns, <<"ns">>}] ) ),
	io:format("~nE: ~p~nB:~p~nR:~p~n~n", [ E, B, R ]),
	?assert( B == R ).

ns_double_quote_test() ->
	E = <<xml, ncn:ns, a:v>>,
	B = <<"<ncn xmlns=\"ns\" a=\"v\"/>">>,
	R = iolist_to_binary( jd_xml:render( E, [{attr_quote, $\"}] ) ),
	io:format("~nE: ~p~nB:~p~nR:~p~n~n", [ E, B, R ]),
	?assert( B == R ).

