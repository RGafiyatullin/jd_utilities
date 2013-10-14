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

-module (jd_xml_sax_test).
-include_lib("jd_xml/include/xml.hrl").
-include_lib("jd_xml/include/sax.hrl").
-include_lib("jd_xml/include/xml_t.hrl").
-include_lib("eunit/include/eunit.hrl").

reassemble_xml_test() ->
	E0 = <<xml, iq:"jabber:client",
			type:set,
			id:"iq123456", [
				<<xml, 'query':"http://localhost/xmpp-binding", [
						<<xml, search:"http://localhost/xmpp-binding", [
							<<chars, "these are chars (portion #1)">>,
							<<cdata, "this is cdata">>,
							<<chars, "these are chars (portion #2)">>
						]>>
					]>>
			]>>,
	EExpected = <<xml, iq:"jabber:client",
			type:set,
			id:"iq123456", [
				<<xml, 'query':"http://localhost/xmpp-binding", [
						<<xml, search:"http://localhost/xmpp-binding", [
							<<cdata, "these are chars (portion #1)">>,
							<<cdata, "this is cdata">>,
							<<cdata, "these are chars (portion #2)">>
						]>>
					]>>
			]>>,
	R = iolist_to_binary(jd_xml:render( E0 )),
	{ok, EReassembled, <<>>} = 
		jd_xml_sax:parse(
			R, jd_xml_sax_aggr:new(),
			fun
				( #sax_document_end{}, Aggregated ) -> Aggregated;
				( Event, Aggr ) ->
					% io:format("Event: ~p~n", [Event]),
					Ret = jd_xml_sax_aggr:event( Event, Aggr ),
					% io:format("Aggr: ~p~n", [ Ret ]),
					{_, Pass} = Ret,
					Pass
			end ),
	io:format("~nGiven:       ~p~n", [ iolist_to_binary( jd_xml:render( E0 ) ) ]),
	io:format("Reassembled: ~p~n", [ iolist_to_binary( jd_xml:render( EReassembled ) ) ]),
	io:format("Expected:    ~p~n", [ iolist_to_binary( jd_xml:render( EExpected ) ) ]),
	?assert( EReassembled == EExpected ).


