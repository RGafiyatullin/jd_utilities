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

-module(jd_xml_sax).
-export([parse/4, parse/3]).

-include("xml.hrl").
-include("sax.hrl").

-type state() :: term().
-type sax_event_handler() :: fun( (sax_event(), state()) -> state() ).
-type data_feed_handler() :: fun( (binary(), state()) -> {binary(), state()} ).
-spec parse( binary(), state(), sax_event_handler(), data_feed_handler() ) -> {ok, state()}.

-record(s,{
		state :: state(),
		sax_event_handler :: sax_event_handler(),
		data_feed_handler :: data_feed_handler()
	}).

parse( InitialInputData, InitialState, SaxEventHandler ) ->
	parse(
		InitialInputData, InitialState, SaxEventHandler,
		fun( _, _ ) -> error({?MODULE,unexpected_end_of_data}) end ).

parse( InitialInputData, InitialState, SaxEventHandler, DataFeedHandler ) ->
	case erlsom:parse_sax(
			InitialInputData,
			#s{ state = InitialState, sax_event_handler = SaxEventHandler, data_feed_handler = DataFeedHandler },
			fun sax_event_handler/2, [ {continuation_function, fun data_feed_handler/2 } ]
		)
	of
		{ok, #s{ state = HandlerState }, DataLeft} -> {ok, HandlerState, jd_utf8:to_string(DataLeft)}
	end.

sax_event_handler( ErlsomSaxEvent, S = #s{
	sax_event_handler = Handler,
	state = HandlerState0
} ) ->
	SaxEvent = parse_erlsom_sax_event( ErlsomSaxEvent ),
	HandlerState1 = Handler( SaxEvent, HandlerState0 ),
	S #s{ state = HandlerState1 }.

data_feed_handler( Tail, S = #s{
	state = HandlerState0,
	data_feed_handler = Handler
} ) ->
	{NewTail, HandlerState1} = Handler( Tail, HandlerState0 ),
	{NewTail, S #s{ state = HandlerState1 }}.

parse_erlsom_sax_event( Evt ) ->
	case Evt of
		startDocument -> #sax_document_start{};
		endDocument -> #sax_document_end{};
		{startPrefixMapping, Prefix, NS} -> #sax_prefix_mapping_start{ prefix = jd_utf8:to_string(Prefix), ns = jd_utf8:to_string(NS) };
		{endPrefixMapping, Prefix} -> #sax_prefix_mapping_end{ prefix = jd_utf8:to_string(Prefix) };
		{startElement, NS, NCN, _Prefix, Attrs} ->
			#sax_element_start{
				ns = jd_utf8:to_string(NS),
				ncn = jd_utf8:to_string(NCN),
				attrs = [
					{jd_utf8:to_string(AttrName), jd_utf8:to_string(AttrValue)}
					|| {attribute, AttrName, _, _, AttrValue} <- Attrs
				] };
		{endElement, NS, NCN, _Prefix} -> #sax_element_end{ ns = jd_utf8:to_string(NS), ncn = jd_utf8:to_string(NCN) };
		{characters, Characters} -> #sax_chars{ data = jd_utf8:to_string(Characters) };
		% CData when is_binary(CData) -> #sax_cdata{ data = CData };
		{ignorableWhitespace, Characters} -> #sax_whitespace{ space = jd_utf8:to_string(Characters) };
		{processingInstruction, Target, Data} -> #sax_processing_instruction{ target = Target, data = Data };
		{error, Description} -> #sax_error{ description = Description };
		{internalError, Description} -> #sax_internal_error{ description = Description }
	end.


