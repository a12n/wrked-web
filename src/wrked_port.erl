-module(wrked_port).

%% API
-export([wrk2fit/1, wrk2fit/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec exec(file:name(), [string() | binary()], iodata()) ->
                  {ok, iodata()} | error | timeout.

exec(Path, Args, Body) ->
    Port = open_port({spawn_executable, Path},
                     [{args, Args}, binary, exit_status]),
    port_command(Port, Body),
    receive_loop(Port, _Ans = []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec wrk2fit(iodata()) -> {ok, iodata()} | error | timeout.

wrk2fit(Wrk) ->
    wrk2fit(Wrk, _Name = undefined, _Sport = undefined).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec wrk2fit(iodata(),
              binary() | undefined,
              binary() | undefined) -> {ok, iodata()} | error | timeout.

wrk2fit(Wrk, Name, Sport) ->
    Args = lists:flatmap(
             fun({_K, _V = undefined}) -> [];
                ({K, V}) -> [K, V] end,
             [ {<<"-name">>, Name},
               {<<"-sport">>, Sport} ]),
    Path = application:get_env(wrked, wrk2fit_path, "bin/wrk2fit"),
    Port = open_port({spawn_executable, Path},
                     [{args, Args}, binary, exit_status]),
    port_command(Port, [Wrk, <<"EOF">>]),
    receive_loop(Port, _Fit = []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

receive_loop(Port, Ans) ->
    receive
        {Port, {data, Data}} -> receive_loop(Port, [Ans, Data]);
        {Port, {exit_status, 0}} -> {ok, Ans};
        {Port, {exit_status, _Code}} -> error
    after 1000 -> port_close(Port), timeout
    end.
