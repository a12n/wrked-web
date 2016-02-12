-module(wrked_handler).

%% Common handler callbacks
-export([init/2]).

%%%===================================================================
%%% Common handler callbacks
%%%===================================================================

init(Req, _Opts) ->
    Name = cowboy_req:binding(name, Req),
    Sport = cowboy_req:binding(sport, Req),
    Spec = cowboy_req:binding(spec, Req),
    Headers = [ {<<"content-type">>, <<"application/vnd.ant.fit">>},
                {<<"content-disposition">>, <<"attachment; filename=xyz.fit">>} ],
    Body = wrk2fit(Name, Sport, Spec),
    Req2 = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, _State = undefined}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

wrk2fit(_Name, _Sport, Spec) ->
    Path = "/home/arn/tmp/a12n/workoued/wrk2ir/wrk2ir.byte",
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
