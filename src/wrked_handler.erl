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
        case wrk2fit(Name, Sport, Wrk) of
            Body when Body =/= undefined ->
                cowboy_req:reply(
                  200, _Headers =
                      [ {<<"content-type">>, ?FIT_MIME_TYPE},
                        {<<"content-disposition">>,
                         [<<"attachment; filename=">>, filename(Name, Sport)]}
                      ],
                  Body, Req)
        end,
    {ok, Req2, _State = undefined}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

filename(_Name = undefined, Sport) ->
    {{Y, M, D}, {H, N, _S}} = erlang:universaltime(),
    [ <<"workout-">>,
      case Sport of
          undefined -> <<>>;
          _Other -> [Sport, $-]
      end,
      io_lib:format("~4..0B~2..0B~2..0BT~2..0B~2..0BZ",
                    [Y, M, D, H, N]),
      <<".fit">> ];

filename(Name, _Sport) ->
    [ <<"workout-">>, Name, <<".fit">> ].

wrk2fit(Name, Sport, Wrk) ->
    Args = lists:flatmap(
             fun({_K, _V = undefined}) -> [];
                ({K, V}) -> [K, V] end,
             [ {<<"-name">>, Name},
               {<<"-sport">>, Sport} ]),
    Path = application:get_env(wrked, wrk2fit_path, "/usr/local/bin/wrk2fit"),
    Port = open_port({spawn_executable, Path},
                     [{args, Args}, binary, stream, use_stdio]),
    port_command(Port, [Wrk, <<"EOF">>]),
    Fit = receive {Port, {data, Data}} -> Data
          after 1000 -> undefined end,
    port_close(Port),
    Fit.
