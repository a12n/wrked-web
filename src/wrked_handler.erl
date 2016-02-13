-module(wrked_handler).

%% Common handler callbacks
-export([init/2]).

-define(FIT_MIME_TYPE, <<"application/vnd.ant.fit">>).

%%%===================================================================
%%% Common handler callbacks
%%%===================================================================

init(Req, _Opts) ->
    Name = cowboy_req:binding(name, Req),
    Sport = cowboy_req:binding(sport, Req),
    Wrk = cowboy_req:binding(wrk, Req),
    Req2 =
        case wrk2fit([{<<"-name">>, Name},
                      {<<"-sport">>, Sport}], Wrk)
        of
            Body when Body =/= undefined ->
                cowboy_req:reply(
                  200, _Headers =
                      [ {<<"content-type">>, ?FIT_MIME_TYPE} ],
                  Body, Req)
        end,
    {ok, Req2, _State = undefined}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

wrk2fit(Args, Wrk) ->
    Args2 = lists:flatmap(
              fun({_K, _V = undefined}) -> [];
                 ({K, V}) -> [K, V] end, Args),
    Path = application:get_env(wrked, wrk2fit_path, "/usr/local/bin/wrk2fit"),
    Port = open_port({spawn_executable, Path},
                     [{args, Args2}, binary, stream, use_stdio]),
    port_command(Port, [Wrk, <<"EOF">>]),
    Fit = receive {Port, {data, Data}} -> Data
          after 1000 -> undefined end,
    port_close(Port),
    Fit.
