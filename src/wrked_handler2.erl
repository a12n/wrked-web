-module(wrked_handler2).

%% Common handler callbacks
-export([init/2]).

%% REST handler callbacks
-export([content_types_provided/2, last_modified/2,
         malformed_request/2]).

%% API
-export([to_fit/2]).

%%%===================================================================
%%% Common handler callbacks
%%%===================================================================

init(Req, _Opts) ->
    {cowboy_rest, Req, _State = undefined}.

%%%===================================================================
%%% REST handler callbacks
%%%===================================================================

content_types_provided(Req, State) ->
    Result = [{{<<"application">>, <<"vnd.ant.fit">>, '*'}, to_fit}],
    {Result, Req, State}.

last_modified(Req, State) ->
    Result = {{2016,05,10}, {22,24,43}},
    {Result, Req, State}.

malformed_request(Req, State) ->
    Name = cowboy_req:binding(name, Req),
    Sport = cowboy_req:binding(sport, Req),
    Wrk = cowboy_req:binding(wrk, Req),
    case wrked_port:wrk2fit(Wrk, Name, Sport) of
        {ok, Fit} ->
            Req2 = cowboy_req:set_resp_header(
                     <<"content-disposition">>,
                     [<<"attachment; filename=">>, filename(Name, Sport)],
                     Req),
            {false, Req2, _State = Fit};
        {error, badarg} ->
            {true, Req, State}
    end.

%%%===================================================================
%%% API
%%%===================================================================

to_fit(Req, State = Fit) -> {Fit, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

filename(Name, Sport) ->
    [<<"workout-">>,
     case Sport of
         undefined -> <<>>;
         _SomeSport -> [Sport, $-]
     end,
     case Name of
         undefined ->
             {{Y, M, D}, {H, N, _S}} = erlang:universaltime(),
             io_lib:format("~4..0B~2..0B~2..0BT~2..0B~2..0BZ",
                           [Y, M, D, H, N]);
         _SomeName -> Name
     end,
     <<".fit">>].
