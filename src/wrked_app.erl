-module(wrked_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    Constr =
        [ { sport,
            fun(Sport) ->
                    lists:member(
                      Sport, [ <<"cycling">>, <<"running">>,
                               <<"swimming">>, <<"walking">> ])
            end }
        ],
    Paths = [ {<<"/workouts/:sport/:name/:wrk">>, Constr, wrked_handler, []},
              {<<"/workouts/:sport/:wrk">>, Constr, wrked_handler, []},
              {<<"/workouts/:wrk">>, wrked_handler, []} ],
    Host = {'_', Paths},
    Dispatch = cowboy_router:compile([Host]),
    Addr = application:get_env(wrked, addr, {127,0,0,1}),
    Port = application:get_env(wrked, port, 8080),
    {ok, _Pid} =
        cowboy:start_http(wrked_http, 100,
                          [{ip, Addr}, {port, Port}],
                          [{compress, true}, {env, [{dispatch, Dispatch}]}]),
    wrked_sup:start_link().

stop(_State) ->
    ok.
