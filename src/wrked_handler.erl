-module(wrked_handler).

%% Common handler callbacks
-export([init/2]).

%%%===================================================================
%%% Common handler callbacks
%%%===================================================================

init(Req, _Opts) ->
    Name = cowboy_req:binding(name, Req),
    Sport = cowboy_req:binding(sport, Req),
    Wrk = cowboy_req:binding(wrk, Req),
    Headers = [ {<<"content-type">>, <<"application/vnd.ant.fit">>},
                {<<"content-disposition">>, <<"attachment; filename=xyz.fit">>} ],
    Body = wrk2fit(Name, Sport, Wrk),
    Req2 = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, _State = undefined}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

wrk2fit(_Name, _Sport, Spec) ->
    Path = application:get_env(wrked, wrk2fit_path, "/usr/local/bin/wrk2fit"),
    Port = open_port({spawn_executable, Path}, [binary, stream, use_stdio]),
    Port ! {self(), {command, [Spec, <<"EOF">>]}},
    Reply =
        receive
            {Port, {data, Data}} ->
                Data
        after 1000 ->
                error(timeout)
        end,
    port_close(Port),
    Reply.
