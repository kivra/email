-module(email).

-export([send/4]).


%%% API ========================================================================


-spec send(To :: binary(), From ::binary(), Subject :: binary(), Message :: binary()) -> ok.
send(To, From, Subject, Message) ->
    SendTo = check_param(To),
    SendFrom = check_param(From),
    gen_server:call(email_controller,
                    {send, SendTo, SendFrom
                     ,ensure_binary(Subject), ensure_binary(Message)}).


%%% Private ========================================================================


check_param({V1, V2}) ->
    {ensure_binary(V1), ensure_binary(V2)};
check_param(Val) ->
    Val2 = ensure_binary(Val),
    {Val2, Val2}.

ensure_binary(B) when is_binary(B) ->
    B;
ensure_binary(L) when is_list(L) ->
    list_to_binary(L);
ensure_binary(A) when is_atom(A) ->
    ensure_binary(atom_to_list(A)).
