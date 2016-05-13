-module(wrked_cache2).

-behaviour(gen_server).

%% API
-export([child_spec/0, start_link/0]).

%% API
-export([fetch/1, store/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec child_spec() -> supervisor:child_spec().

child_spec() ->
    #{id => ?MODULE, start => {?MODULE, start_link, []}}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(binary()) -> {ok, iodata()} | error.

fetch(Wrk) ->
    gen_server:call(?MODULE, {fetch, Wrk}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store(binary(), iodata()) -> ok.

store(Wrk, Fit) ->
    gen_server:cast(?MODULE, {store, Wrk, Fit}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
init(_Opts) ->
    {ok, _State = undefined}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_call({fetch, Wrk}, _From, State) ->
    Reply = case file:read_file(filename(Wrk)) of
                {ok, Fit}        -> {ok, Fit};
                {error, _Reason} -> error
            end,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_cast({store, Wrk, Fit}, State) ->
    file:write_file(filename(Wrk), Fit),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

filename(Wrk) ->
    filename:join(binary:split(Wrk, [<<",">>, <<";">>],
                               [global, trim_all])).
