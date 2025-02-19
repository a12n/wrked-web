-module(wrked_port).

%% API
-export([il2fit/1, wrk2fit/1, wrk2fit/3, wrk2il/2, wrk2il/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec il2fit(iodata()) -> {ok, iodata()} | error | timeout.

il2fit(Il) ->
    exec(
      _Path = application:get_env(wrked, il2fit_path, "bin/il2fit"),
      _Args = [],
      _Body = [Il, <<"EOF\n">>]
     ).

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
    case wrk2il(Wrk, translate, Name, Sport) of
        {ok, Il} -> il2fit(Il);
        error    -> error;
        timeout  -> timeout
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec wrk2il(iodata(), translate | minimize) ->
                    {ok, iodata()} | error | timeout.

wrk2il(Wrk, Mode) ->
    wrk2il(Wrk, Mode, _Name = undefined, _Sport = undefined).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec wrk2il(iodata(), translate | minimize,
             binary() | undefined,
             binary() | undefined) -> {ok, iodata()} | error | timeout.

wrk2il(Wrk, Mode, Name, Sport) ->
    exec(
      _Path = application:get_env(wrked, wrk2il_path, "bin/wrk2il"),
      _Args = lists:flatmap(
                fun({_K, _V = undefined}) -> [];
                   ({K, V}) -> [K, V] end,
                [ {<<"-name">>, Name},
                  {<<"-sport">>, Sport},
                  {<<"-mode">>,
                   case Mode of
                       translate -> <<"tr">>;
                       minimize  -> <<"min">>
                   end} ]),
      _Body = [Wrk, <<"EOF">>]
     ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec exec(file:name(), [string() | binary()], iodata()) ->
                  {ok, iodata()} | error | timeout.

exec(Path, Args, Body) ->
    Port = open_port({spawn_executable, Path},
                     [{args, Args}, binary, exit_status]),
    port_command(Port, Body),
    receive_loop(Port, _Ans = <<>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec receive_loop(port(), iodata()) -> {ok, iodata()} | error | timeout.

receive_loop(Port, Ans) ->
    receive
        {Port, {data, Data}} -> receive_loop(Port, [Ans, Data]);
        {Port, {exit_status, 0}} -> {ok, Ans};
        {Port, {exit_status, _Code}} -> error
    after 1000 -> port_close(Port), timeout
    end.
