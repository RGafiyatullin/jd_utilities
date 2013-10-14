-ifndef(jd_utils_include_gen_server_hrl).
-define(jd_utils_include_gen_server_hrl, true).

-type gen_reply_to() :: {pid(), reference()}.
-type gen_server_init_ret( State_t ) :: {ok, State_t} | {ok, State_t, timeout() | hibernate}.
-type gen_server_call_ret( Reply_t, State_t ) ::
	{reply, Reply_t, State_t} | {reply, Reply_t, State_t, timeout() | hibernate} |
	{noreply, State_t} | {noreply, State_t, timeout() | hibernate} |
	{stop, Reason :: term(), Reply_t, State_t} |
	{stop, Reason :: term(), State_t}.
-type gen_server_cast_ret( State_t ) ::
	{noreply, State_t} | {noreply, State_t, timeout() | hibernate} |
	{stop, Reason :: term(), State_t}.
-type gen_server_info_ret( State_t ) :: gen_server_cast_ret( State_t ).
-type gen_server_terminate_ret() :: ignore.
-type gen_server_code_change_ret( State_t ) :: {ok, State_t} | {error, Reason :: term()}.



-endif. %% rg_utils_include_gen_server_hrl
