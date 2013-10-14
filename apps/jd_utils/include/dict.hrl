-ifndef(jd_utils_include_dict_hrl).
-define(jd_utils_include_dict_hrl, true).

-define(dict_t, orddict:orddict).
-define(dict, orddict).

-type nillable( Smth_t ) :: undefined | Smth_t.
-type maybe( Smth_t ) :: {ok, Smth_t} | {error, Reason :: term()}.

-endif. %% jd_utils_include_dict_hrl
