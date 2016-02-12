-module(wrked_handler).

%% Common handler callbacks
-export([init/2]).

%%%===================================================================
%%% Common handler callbacks
%%%===================================================================

init(Req, _Opts) ->
    Headers = [ {<<"content-type">>, <<"application/vnd.ant.fit">>},
                {<<"content-disposition">>, <<"attachment; filename=xyz.fit">>} ],
    Body = <<>>,                                % TODO
    Req2 = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, _State = undefined}.
