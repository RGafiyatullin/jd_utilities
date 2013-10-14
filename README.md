# Overview

jd_utilities is a set of tools which can be used acrossed my erlang projects:

* time manipulation utils (jd_time)
* conversion between binariy and list representations of UTF8 strings (jd_utf8)
* shaping (throttling) utilities (jd_exp_backoff_shaper, jd_token_bucket)
* parse transform helpers (jd_pt)
* first-class support for XML in Erlang code ( {parse_transform, jd_xml_t} )

# XML utils

XML is stored in the following records:

* \#xe{ ncn, ns, a, c } - XML-element
* \#xc{ data } - XML character data (text-node)
* \#xd{ data } - XML CDATA-node

Those however are not intended to be used directly.
The following PT-sugar is available:
```erlang
%% use xml_t parse transform
-include_lib("jd_xml/include/xml_t.hrl").
%% ...
xml_big_test() ->
	MessaageBody = jd_xml:add_ch( <<"Hello">>, ( jd_xml:e(<<"jabber-client">>,<<"body">>) ) ),
	MessageOld = 
		jd_xml:add_ch( MessaageBody,
			jd_xml:set_attrs( [
					{<<"to">>,<<"julliet@capulet.shakespeare.lit">>},
					{<<"from">>, <<"romeo@montague.shakespeare.lit">>},
					{<<"type">>, <<"chat">>}
				], jd_xml:e(<<"jabber-client">>, <<"message">>) ) ),
	MessageNew = 
		<<xml, message:"jabber-client",
			type:chat,
			from:<<"romeo@montague.shakespeare.lit">>,
			to:<<"julliet@capulet.shakespeare.lit">>,
			[
				<<xml, body:"jabber-client", [
						<<cdata, "Hello">>
					]>>
			]>>,
	io:format("~nold:~p~nnew:~p~n", [MessageOld, MessageNew]),
	?assert( MessageNew == MessageOld ).
```
Since the latter XML-syntax is converted into a record-construction statements
(as opposed to function calls),
it can be used in pattern matching also:
```erlang
ast_case_test() ->
	?assert(
		case <<xml, ncn:ns>> of
			<<xml, ncn1:ns1>> -> false;
			<<xml, ncn:ns>> -> true;
			_ -> false
		end
	).
ast_try4_test() ->
	?assert(
		try
			throw(<<xml, error:"http://etherx.jabber.org/streams", [
					<<xml, 'internal-server-error':"urn:ietf:params:xml:ns:xmpp-streams">>
				]>>)
		catch
			throw:<<xml, error:"http://etherx.jabber.org/streams", [ Condition | _ ]>> ->
				io:format("stream-error condition: ~p~n", [ Condition ]),
				true;
			throw:SomethingElse ->
				io:format("got something else:~p~n", [SomethingElse]),
				false
		end).
```

Render XML, parse it (SAX parser backend: erlsom), reassemble XML structure from SAX-events:
```erlang
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
```

