-type child()    :: 'undefined' | pid().
-type child_id() :: term().
-type mfargs()   :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type modules()  :: [module()] | 'dynamic'.
-type restart()  :: 'permanent' | 'transient' | 'temporary'.
-type shutdown() :: 'brutal_kill' | timeout().
-type worker()   :: 'worker' | 'supervisor'.
-type sup_name() :: {'local', Name :: atom()} | {'global', Name :: atom()}.
-type sup_ref()  :: Name :: atom()
                  | {Name :: atom(), Node :: node()}
                  | {'global', Name :: atom()}
                  | pid().
-type child_spec() :: {Id :: child_id(),
                       StartFunc :: mfargs(),
                       Restart :: restart(),
                       Shutdown :: shutdown(),
                       Type :: worker(),
                       Modules :: modules()}.

-type strategy() :: 'one_for_all' | 'one_for_one'
                  | 'rest_for_one' | 'simple_one_for_one'.

-spec init(Args :: term()) ->
    {ok, {{RestartStrategy :: strategy(),
           MaxR            :: non_neg_integer(),
           MaxT            :: non_neg_integer()},
           [ChildSpec :: child_spec()]}}
    | ignore.
