-ifndef(durden_xml_hrl).
-define(durden_xml_hrl, included).

-type xml_ns() :: string().
-type xml_ncname() :: string().
-type xml_qname() :: { xml_ns(), xml_ncname() }.

-endif. % durden_xml_hrl
