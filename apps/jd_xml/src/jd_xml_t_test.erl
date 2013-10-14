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

-module(jd_xml_t_test).

-include_lib("jd_xml/include/xml_t.hrl").
-include_lib("eunit/include/eunit.hrl").

xml_cdata_test() ->
	Old = jd_xml:d(<<"Hello!">>),
	New = <<cdata, "Hello!">>,
	io:format("~nold:~p~nnew:~p~n", [Old, New]),
	?assert( Old == New ).
xml_chars_test() ->
	Old = jd_xml:c(<<"Hello!">>),
	New = <<chars, "Hello!">>,
	io:format("~nold:~p~nnew:~p~n", [Old,New]),
	?assert( Old == New ).
xml_element1_test() ->
	Old = jd_xml:e(<<"ns">>, <<"ncn">>),
	New = <<xml, ncn:ns>>,
	io:format("~nold:~p~nnew:~p~n", [Old, New]),
	?assert( Old == New ).
xml_element2_test() ->
	Old = jd_xml:e(<<"ns">>, <<"ncn">>),
	New = <<xml, "ncn":"ns">>,
	io:format("~nold:~p~nnew:~p~n", [Old, New]),
	?assert( Old == New ).
xml_element3_test() ->
	Old = jd_xml:set_attrs( [{<<"a2">>, <<"v2">>},{<<"a1">>,<<"v1">>}], jd_xml:e(<<"ns">>,<<"ncn">>) ),
	New = <<xml, ncn:ns, a1:<<"v1">>, a2:v2 >>,
	io:format("~nold:~p~nnew:~p~n", [Old, New]),
	?assert( Old == New ).
xml_element4_test() ->
	Old = jd_xml:add_chs( [jd_xml:e(<<"ns1">>, <<"ncn1">>), jd_xml:e(<<"ns2">>,<<"ncn2">>)], jd_xml:e(<<"ns">>,<<"ncn">>) ),
	New = <<xml, ncn:ns, [ <<xml, ncn1:ns1>>, <<xml, ncn2:ns2>> ]>>,
	io:format("~nold:~p~nnew:~p~n", [Old, New]),
	?assert( Old == New ).

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

ast_case1_test() ->
	?assert(
		case <<xml, ncn:ns>> of
			<<xml, ncn1:ns1>> -> false;
			<<xml, ncn:ns>> -> true;
			_ -> false
		end
	).

ast_case2_test() ->
	?assert(
		case <<chars, "chars">> of
			<<chars, Chars>> when Chars == <<"chars">> -> true;
			<<chars, _SomethingElse>> -> false;
			Wtf -> {wtf, Wtf}
		end ).

ast_case3_test() ->
	?assert(
		case <<cdata, "chars">> of
			<<cdata, Chars>> when Chars == <<"chars">> -> true;
			<<cdata, _SomethingElse>> -> false;
			Wtf -> {wtf, Wtf}
		end ).

ast_case4_test() ->
	String = "string",
	CData = <<cdata, (list_to_binary(String))>>,
	<<cdata, Binary>> = CData,
	?assert(binary_to_list(Binary) == String).


ast_if_test() ->
	?assert(
		if
			true ->
				Old = jd_xml:e(<<"ns">>, <<"ncn">>),
				New = <<xml, ncn:ns>>,
				io:format("~nold:~p~nnew:~p~n", [Old, New]),
				?assert(Old == New),
				true
		end
	).

ast_catch_test() ->
	Old = jd_xml:e( <<"ns">>, <<"ncn">> ),
	New = (catch <<xml, ncn:ns>>),
	io:format("~nold:~p~nnew:~p~n", [Old, New]),
	?assert( Old == New ).

ast_try0_test() ->
	try
		Old = jd_xml:e( <<"ns">>, <<"ncn">> ),
		New = <<xml, ncn:ns>>,
		io:format("~nold:~p~nnew:~p~n", [Old, New]),
		?assert( Old == New )
	catch
		nothing -> ok
	end.

ast_try1_test() ->
	Old = jd_xml:e( <<"ns">>, <<"ncn">> ),
	New = try <<xml, ncn:ns>> catch nothing -> ok end,
	io:format("~nold:~p~nnew:~p~n", [Old, New]),
	?assert( Old == New ).

ast_try2_test() ->
	try
		throw(catch_me)
	catch
		throw:catch_me ->
			Old = jd_xml:e( <<"ns">>, <<"ncn">> ),
			New = <<xml, ncn:ns>>,
			io:format("~nold:~p~nnew:~p~n", [Old, New]),
			?assert( Old == New )
	end.

ast_try3_test() ->
	try
		ok
	catch
		nothing -> ok
	after
		Old = jd_xml:e( <<"ns">>, <<"ncn">> ),
		New = <<xml, ncn:ns>>,
		io:format("~nold:~p~nnew:~p~n", [Old, New]),
		?assert( Old == New )
	end.

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

-record(r, {
		f1 :: term(),
		f2 :: term()
	}).
ast_record0_test() ->
	R = #r{
		f1 = jd_xml:e(<<"ns">>, <<"ncn">>),
		f2 = <<xml, ncn:ns>>
	},
	io:format("~nold:~p~nnew:~p~n", [R#r.f1, R#r.f2]),
	?assert( R#r.f1 == R#r.f2 ).

ast_record1_test() ->
	R = #r{
		f1 = jd_xml:e(<<"ns">>, <<"ncn">>)
	} #r{
		f2 = <<xml, ncn:ns>>
	},
	io:format("~nold:~p~nnew:~p~n", [R#r.f1, R#r.f2]),
	?assert( R#r.f1 == R#r.f2 ).

ast_list_comprehension_test() ->
	Expected = [
		<<xml, ncn1:ns1, b:bee, c:cee, []>>,
		<<xml, ncn2:ns2, b:bee, c:cee, []>>
	],
	L = [ {<<"ns1">>, <<"ncn1">>}, {<<"ns2">>, <<"ncn2">>} ],
	Actual = [ <<xml, NCN:NS, b:bee, c:cee>> || {NS, NCN} <- L ],
	io:format("Expected: ~p~nActual: ~p~n", [ Expected, Actual ]),
	?assert( Expected == Actual ).


