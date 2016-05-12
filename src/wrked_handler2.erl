-module(wrked_handler2).

%% Common handler callbacks
-export([init/2]).

%% REST handler callbacks
-export([content_types_provided/2, last_modified/2,
         malformed_request/2]).

%% API
-export([to_fit/2]).

-record(state, {
          name  :: binary(),
          sport :: binary(),
          wrk   :: binary(),
          fit   :: iodata()
         }).

%%%===================================================================
%%% Common handler callbacks
%%%===================================================================

init(Req, _Opts) ->
    Name = cowboy_req:binding(name, Req),
    Sport = cowboy_req:binding(sport, Req),
    Wrk = cowboy_req:binding(wrk, Req),
    Fit = case wrked_cache:fetch(Wrk) of
              {ok, Cached} -> Cached;
              error        -> undefined
          end,
    State = #state{name = Name, sport = Sport,
                   wrk = Wrk, fit = Fit},
    {cowboy_rest, Req, State}.

%%%===================================================================
%%% REST handler callbacks
%%%===================================================================

content_types_provided(Req, State) ->
    Result = [{{<<"application">>, <<"vnd.ant.fit">>, '*'}, to_fit}],
    {Result, Req, State}.

last_modified(Req, State) ->
    Result = {{2016,05,10}, {22,24,43}},
    {Result, Req, State}.

%% Fetched from the cache, Wrk was valid
malformed_request(Req, State = #state{fit = Fit})
  when Fit =/= undefined ->
    {false, Req, State};

%% Not in the cache yet, Wrk may be malformed
malformed_request(Req, State = #state{wrk = Wrk, name = Name,
                                      sport = Sport}) ->
    case wrked_port:wrk2il(Wrk, minimize, Name, Sport) of
        {ok, RawMinWrk} ->
            %% Proper Wrk, was able to minimize
            MinWrk = iolist_to_binary(RawMinWrk),
            Fit = case wrked_cache:fetch(MinWrk) of
                      {ok, Cached} -> Cached;
                      error        -> undefined
                  end,
            Req2 = cowboy_req:set_resp_header(
                     <<"content-disposition">>,
                     [<<"attachment; filename=">>,
                      filename(Name, Sport)],
                     Req),
            {false, Req2, State#state{wrk = MinWrk, fit = Fit}};
        error ->
            %% Malformed Wrk
            {true, Req, State}
    end.

%%%===================================================================
%%% API
%%%===================================================================

%% Wrk or MinWrk was in the cache, Fit is already fetched
to_fit(Req, State = #state{fit = Fit})
  when Fit =/= undefined ->
    {Fit, Req, State};

%% Wasn't cached, generate Fit
to_fit(Req, State = #state{wrk = Wrk}) ->
    {ok, Fit} = wrked_port:wrk2fit(Wrk),
    wrked_cache:store(Wrk, Fit),
    {Fit, Req, State}.

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
