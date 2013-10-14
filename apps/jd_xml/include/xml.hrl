-ifndef( jd_xml_include_xml_hrl ).
-define( jd_xml_include_xml_hrl, true ).

-type xml_ncn() :: binary().
-type xml_ns() :: binary().

% -type xml_ns_prefix() :: binary().
% -type xml_ns_import() :: { xml_ns(), xml_ns_prefix() }.

-record(xe, {
		ncn = throw({not_nil, 'xe.ncn'}) :: xml_ncn(),
		ns = throw({not_nil, 'xe.ns'}) :: xml_ns(),
		a = [] :: [ {xml_ncn(), binary()} ],
		c = [] :: [ xml_element() ]
		% ,
		% i = [] :: [ xml_ns_import() ]
	}).
-record(xc, {
		data = <<>> :: binary()
	}).
-record(xd, {
		data = <<>> :: binary()
	}).

-type xml_element() :: #xe{} | #xc{} | #xd{}.
-define(is_xml_element( X ), is_record( X, xe ) orelse is_record( X, xc ) orelse is_record( X, xd ) ).

-endif. %% jd_xml_include_xml_hrl
