
-type cowboy_handler_opts() :: any().
-type cowboy_handler_state() :: any().

-spec init({atom(), http}, Req, cowboy_handler_opts())
	-> {ok, Req, cowboy_handler_state()}
	| {loop, Req, cowboy_handler_state()}
	| {loop, Req, cowboy_handler_state(), hibernate}
	| {loop, Req, cowboy_handler_state(), timeout()}
	| {loop, Req, cowboy_handler_state(), timeout(), hibernate}
	| {shutdown, Req, cowboy_handler_state()}
	| {upgrade, protocol, module()}
	when Req::cowboy_req:req().
-spec handle(Req, State) -> {ok, Req, State}
	when Req::cowboy_req:req(), State::cowboy_handler_state().
-spec terminate(cowboy_req:req(), cowboy_handler_state()) -> ok.