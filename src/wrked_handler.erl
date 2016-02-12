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

wrk2fit(_Name, _Sport, _Spec) ->
    %% TODO
    <<>>.
