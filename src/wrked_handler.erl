-module(wrked_handler).

%% Common handler callbacks
-export([init/2]).

%% REST handler callbacks
-export([content_types_provided/2]).

%%%===================================================================
%%% Common handler callbacks
%%%===================================================================

init(Req, Opts) -> {cowboy_rest, Req, Opts}.

%%%===================================================================
%%% REST handler callbacks
%%%===================================================================

content_types_provided(Req, State) -> {_Result = [], Req, State}.
