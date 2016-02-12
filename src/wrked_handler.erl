-module(wrked_handler).

%% Common handler callbacks
-export([init/2]).

%%%===================================================================
%%% Common handler callbacks
%%%===================================================================

init(Req, _Opts) ->
    _Name = cowboy_req:binding(name, Req),
    _Sport = cowboy_req:binding(sport, Req),
    _Spec = cowboy_req:binding(spec, Req),
    Headers = [ {<<"content-type">>, <<"application/vnd.ant.fit">>},
                {<<"content-disposition">>, <<"attachment; filename=xyz.fit">>} ],
    Body = <<>>,                                % TODO
    Req2 = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, _State = undefined}.
