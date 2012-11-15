%% ----------------------------------------------------------------------------
%%
%% email: Erlang mail application
%%
%% Copyright (c) 2012 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
%% ----------------------------------------------------------------------------

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
    To = <<ToName/binary, " <", ToEmail/binary, ">">>,
    From = <<FromName/binary, " <", FromEmail/binary, ">">>,
    Url = restc:construct_url(Conn#state.apiurl, "messages", []),
    Body = [{<<"to">>, To},
            {<<"from">>, From},
            {<<"text">>, Message},
            {<<"subject">>, Subject}],

    case restc:request(post, percent, Url, [200], Auth, Body) of
        {ok, _, _, Payload} ->
            {ok, Payload};
        {error, _, _, Payload} ->
            {error, Payload};
        {error, Payload} ->
            {error, Payload}
    end.


%%% Private ========================================================================


get_apiurl(Domain, Url) ->
    restc:construct_url(Url, Domain, []).

auth_header(User, Password) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Password])),
    [{"Authorization", "Basic " ++ Encoded}].

