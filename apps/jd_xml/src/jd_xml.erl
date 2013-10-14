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

-module(jd_xml).
-compile({parse_transform, cut}).
-compile({parse_transform, gin}).
-compile({parse_transform, jd_xml_t}).
-export([
		render/1, render/2,

		e/2, ns/1, ncn/1,
		c/1, d/1,

		attr/2, attrs/1,
		set_attr/3, set_attrs/2,

		ch/1, add_ch/2, add_ch_front/2,
		set_chs/2, add_chs/2,

		text/1,
		flat_text/1
	]).
-export_type([
		xml/0
	]).
-include("xml.hrl").
-opaque xml() :: #xe{} | #xc{} | #xd{}.

-spec e( NS :: xml_ns(), NCN :: xml_ncn() ) -> xml().
-spec ns( #xe{} ) -> xml_ns().
-spec ncn( #xe{} ) -> xml_ncn().

-spec attr( xml_ncn(), #xe{} ) -> binary() | undefined.
-spec attrs( #xe{} ) -> [ {xml_ncn(), binary()} ].
-spec set_attr( xml_ncn(), binary(), #xe{} ) -> #xe{}.
-spec set_attrs( [{xml_ncn(), binary()}], #xe{} ) -> #xe{}.
-spec render( xml() ) -> iolist().


render( Xml ) -> render( Xml, [] ).
render( Xml, Opts ) -> jd_xml_render:render( Xml, Opts ).

e( NS, NCN ) when is_binary( NS ) andalso is_binary(NCN) -> #xe{ ns = NS, ncn = NCN }.
c( Chars ) when is_binary( Chars ) -> #xc{ data = Chars }.
d( Data ) when is_binary( Data ) -> #xd{ data = Data }.
ns( #xe{ ns = NS } ) -> NS.
ncn( #xe{ ncn = NCN } ) -> NCN.

attr( K, #xe{ a = Attrs } ) when is_binary( K ) -> proplists:get_value( K, Attrs, undefined ).
attrs( #xe{ a = Attrs } ) -> Attrs.

set_attr( K, undefined, X = #xe{ a = Attrs } ) when is_binary(K) -> X #xe{ a = lists:filter( fun({AName, _}) -> AName /= K end, Attrs ) };
set_attr( K, V, X = #xe{ a = Attrs } ) when is_binary(K) and is_binary(V) -> X #xe{ a = [ {K, V} | lists:filter( fun({AName, _}) -> AName /= K end, Attrs ) ] }.
set_attrs( [], X = #xe{} ) -> X;
set_attrs( [ {K, V} | AttrsSoFar ], X ) -> set_attrs( AttrsSoFar, set_attr(K, V, X) ).

ch( #xe{ c = Chs } ) -> Chs.

add_ch( Binary, X = #xe{} ) when is_binary(Binary) -> add_ch( d(Binary), X );
add_ch( Ch, X = #xe{ c = OldChs } ) when ?is_xml_element(Ch) -> X #xe{ c = OldChs ++ [ Ch ] }.

add_ch_front( Binary, X = #xe{} ) when is_binary(Binary) -> add_ch_front( d(Binary), X );
add_ch_front( Ch, X = #xe{ c = OldChs } ) when ?is_xml_element( Ch ) -> X #xe{ c = [ Ch | OldChs] }.

set_chs( Chs, X = #xe{} ) -> add_chs( Chs, X #xe{ c = [] } ).
add_chs( Chs, X = #xe{ c = OldChs } ) when is_list( Chs ) ->
	ChsModified = 
		lists:map( fun
				( ChXml ) when ?is_xml_element( ChXml ) -> ChXml;
				( Binary ) when is_binary( Binary ) -> d(Binary);
				( NotXml ) -> error( {badarg, NotXml} )
			end,
			Chs ),
	X #xe{ c = OldChs ++ ChsModified }.


-spec flat_text( xml_element() ) -> binary().
flat_text( Xml ) -> iolist_to_binary( text( Xml ) ).

-spec text( xml_element() ) -> iolist().
text( <<xml, _:_, Children >> ) ->
	lists:map( fun text/1, Children );
text( <<cdata, Data>> ) ->
	Data;
text( <<chars, Data>> ) ->
	Data.


%%% %%%%% %%%
%%% Tests %%%
%%% %%%%% %%%
-include_lib("eunit/include/eunit.hrl").
t0_test() ->
	E = ?MODULE:e( <<"ns">>, <<"ncn">> ),
	?assert( ?MODULE:ns(E) == <<"ns">> ),
	?assert( ?MODULE:ncn(E) == <<"ncn">> ),
	?assert( ?MODULE:attrs(E) == [] ),
	?assert( ?MODULE:ch(E) == [] ).

t1_test() ->
	E0 = ?MODULE:e( <<"ns">>, <<"ncn">> ),
	E1 = ?MODULE:set_attrs( [{<<"k1">>, <<"_">>}, {<<"k2">>, <<"v2">>}, {<<"k1">>, <<"v1">>}], E0 ),
	?assert( ?MODULE:attr( <<"k2">>, E1 ) == <<"v2">> ),
	?assert( ?MODULE:attr( <<"k1">>, E1 ) == <<"v1">> ),
	E2 = ?MODULE:set_attr( <<"k1">>, undefined, E1 ),
	?assert( ?MODULE:attr( <<"k2">>, E2 ) == <<"v2">> ),
	?assert( ?MODULE:attr( <<"k1">>, E2 ) == undefined ),
	?assert( ?MODULE:ch( E0 ) == [] ),
	?assert( ?MODULE:ch( E1 ) == [] ),
	?assert( ?MODULE:ch( E2 ) == [] ).

t2_test() ->
	E0 = ?MODULE:e( <<"ns">>, <<"ncn">> ),
	E0_1 = ?MODULE:e( <<"ns01">>, <<"ncn01">> ),
	E0_2 = ?MODULE:e( <<"ns02">>, <<"ncn02">> ),
	E1 = ?MODULE:add_ch( E0_1, E0 ),
	E2 = ?MODULE:add_chs( [ E0_2, E0_1 ], E1 ),
	?assert( ?MODULE:ch( E0 ) == [] ),
	?assert( ?MODULE:ch( E1 ) == [ E0_1 ] ),
	?assert( ?MODULE:ch( E2 ) == [ E0_1, E0_2, E0_1 ] ).


