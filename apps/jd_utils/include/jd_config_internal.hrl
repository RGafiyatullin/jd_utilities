-ifndef(jd_config_internal_hrl).
-define(jd_config_internal_hrl, true).

-record( option, {
		line :: non_neg_integer(),
		name :: atom(),
		default :: false | { defined, term() },
		props = [] :: [ term() ]
	} ).

-record(i, {
		mod = undefined :: atom(),
		app = undefined :: atom(),
		options = [] :: [],
		errors = [] :: []
	}).

-define(defs_callback, '#jd_config.defs#').

-endif. % jd_config_internal_hrl
