-module(email_adapter_mailgun).

-behaviour(email_adapter).

-export([start/0, start/1, stop/1]).
-export([send/5]).

-record(state, {
		apiurl :: string(),
		apikey :: string()
        }).


%%% API ========================================================================


start() ->
    start([]).

start(Options) ->
    application:start(inets),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    Domain = proplists:get_value(domain, Options),
    ApiUrl = get_apiurl(Domain, proplists:get_value(apiurl, Options)),
    ApiKey = proplists:get_value(apikey, Options),
    {ok, #state{apiurl=ApiUrl, apikey=ApiKey}}.

stop(_Conn) ->
    ok.

send(Conn, {ToName, ToEmail}, {FromName, FromEmail}, Subject, Message) ->
    Auth = auth_header("api", Conn#state.apikey),
    To = ToName++" <"++ToEmail++">",
    From = FromName++" <"++FromEmail++">",
    Url = restc:construct_url(Conn#state.apiurl, "messages", []),
    Body = [{to, To},
            {from, From},
            {text, Message},
            {subject, Subject}],

    case restc:request(post, percent, Url, [200], Auth, Body) of
        {ok, _, _, Payload} ->
            {ok, Payload};
        {error, _, _, Payload} ->
            {error, Payload};
        {error, Payload} ->
            {error, Payload}
    end.


%%% API ========================================================================


get_apiurl(Domain, Url) ->
    restc:construct_url(Url, Domain, []).

auth_header(User, Password) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Password])),
    [{"Authorization", "Basic " ++ Encoded}].

