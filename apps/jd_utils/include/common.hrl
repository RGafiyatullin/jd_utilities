-ifndef( jd_utils_include_common_hrl ).
-define( jd_utils_include_common_hrl, true ).

-define( i2b( I ), list_to_binary( integer_to_list( I ) ) ).
-define( b2i( B ), list_to_integer( binary_to_list( B ) ) ).

-endif. % jd_utils_include_common_hrl
