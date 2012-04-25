-module(email).

-export([send/4]).


%%% API ========================================================================


send(To, From, Subject, Message) ->
    SendTo = check_param(To),
    SendFrom = check_param(From),
    gen_server:call(email_controller, {send, SendTo, SendFrom, Subject, Message}).

check_param(Val) when is_tuple(Val) ->
    Val;
check_param(Val) ->
    {[], Val}.

