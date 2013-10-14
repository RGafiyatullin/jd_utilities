-ifndef(jd_xml_include_sax_hrl).
-define(jd_xml_include_sax_hrl, true).

-include_lib("jd_xml/include/xml.hrl").

-record(sax_document_start, {}).
-record(sax_document_end, {}).
-record(sax_prefix_mapping_start, { prefix :: xml_ncn(), ns :: xml_ns() }).
-record(sax_prefix_mapping_end, { prefix :: xml_ncn() }).
-record(sax_element_start, { ns :: xml_ns(), ncn :: xml_ncn(), attrs :: [ {xml_ncn(), binary()} ] }).
-record(sax_element_end, { ns :: xml_ns(), ncn :: xml_ncn() }).
-record(sax_chars, { data :: binary() }).
% -record(sax_cdata, { data :: binary() }).
-record(sax_whitespace, { space :: binary() }).
-record(sax_processing_instruction, { target, data }).
-record(sax_error, { description :: term() }).
-record(sax_internal_error, { description :: term() }).

-type sax_event() ::
	  #sax_document_start{} | #sax_document_end{}
	| #sax_prefix_mapping_start{} | #sax_prefix_mapping_end{}
	| #sax_element_start{} | #sax_element_end{}
	| #sax_chars{} 
	% | #sax_cdata{} 
	| #sax_whitespace{}
	| #sax_processing_instruction{} | #sax_error{} | #sax_internal_error{}.

-endif. % jd_xml_include_sax_hrl
