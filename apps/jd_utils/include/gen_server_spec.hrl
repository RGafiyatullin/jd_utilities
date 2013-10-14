
-include_lib("jd_utils/include/gen_server.hrl").

-spec init(Args :: term()) -> gen_server_init_ret( term() ).
-spec handle_call(
	Request :: term(),
	From :: gen_reply_to(),
	State :: term()
) ->
	gen_server_call_ret( term(), term() ).

-spec handle_cast(Request :: term(), State :: term()) -> gen_server_cast_ret( term() ).
-spec handle_info(Info :: timeout | term(), State :: term()) -> gen_server_info_ret( term() ).
-spec terminate(
	Reason :: normal | shutdown | {shutdown, term()} | term(),
	State :: term()
) ->
	gen_server_terminate_ret().

-spec code_change(
	OldVsn :: term() | {down, term()},
	State :: term(),
	Extra :: term()
) ->
	gen_server_code_change_ret( term() ).

