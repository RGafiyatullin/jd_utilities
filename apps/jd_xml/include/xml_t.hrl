-ifndef(jd_xml_include_xml_t).
-define(jd_xml_include_xml_t, true).

-compile({parse_transform, jd_xml_t}).
-include_lib("jd_xml/include/xml.hrl").

-endif. % jd_xml_include_xml_t
