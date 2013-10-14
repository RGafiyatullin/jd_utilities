-ifndef(jd_hooks_include_jd_hooks_hrl).
-define(jd_hooks_include_jd_hooks_hrl, true).

-type hook_scope() :: local | global.
-type hook_handler()
	:: {mfa, M :: atom(), F :: atom(), A :: [term()]} % erlang:apply( M, F, A ++ [ HookAcc | HookArgs ] )
	|  {function, F :: fun() }. % erlang:apply( F, [ HookAcc | HookArgs ] )

-endif. % jd_hooks_include_jd_hooks_hrl
