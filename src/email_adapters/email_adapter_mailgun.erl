%% ----------------------------------------------------------------------------
%%
%% email: Erlang mail application
%%
%% Copyright (c) 2012-2013 KIVRA
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

-export([start/0]).
-export([start/1]).
-export([stop/1]).
-export([send/6]).
-export([url_encode/1]).

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
    ApiUrl = proplists:get_value(apiurl, Options),
    ApiKey = proplists:get_value(apikey, Options),
    {ok, #state{apiurl=ApiUrl++"/"++Domain, apikey=ApiKey}}.

stop(_Conn) ->
    ok.

send(Conn, {ToName, ToEmail}, {FromName, FromEmail}, Subject, Message, Opt) ->
    Body0 = [{<<"to">>, <<ToName/binary, " <", ToEmail/binary, ">">>},
             {<<"from">>, <<FromName/binary, " <", FromEmail/binary, ">">>},
             {<<"subject">>, Subject}],
    Body1 = add_message(lists:merge(Opt, Body0), Message),

    case httpc:request( post, construct_request(Conn, Body1)
                      , [], [{body_format, binary}] ) of
        {ok, {{_, 200, _}, _, Payload}} -> {ok, Payload};
        {ok, {{_, _, _}, _, Payload}}   -> {error, Payload};
        Error                           -> Error
    end.


%%% Private ========================================================================


add_message(Body, {html, Message}) ->
    [{<<"html">>, Message} | Body];
add_message(Body, {text, Message}) ->
    [{<<"text">>, Message} | Body];
add_message(Body, Message) ->
    add_message(Body, {text, Message}).

construct_request(Conn, Body) ->
    { Conn#state.apiurl++"/"++"messages"
    , auth_header("api", Conn#state.apikey)
    , "application/x-www-form-urlencoded"
    , url_encode(Body) }.

auth_header(User, Password) ->
    [{ "Authorization"
     , "Basic "++base64:encode_to_string(lists:append([User,":",Password])) }].

url_encode(Data)    -> url_encode(Data, <<"">>).

url_encode([], Acc) -> Acc;
url_encode([{Key, Value} | T], <<"">>) ->
    url_encode(T, << (escape_uri(Key))/binary
                   , $=, (escape_uri(Value))/binary >>);
url_encode([{Key, Value} | T], Acc) ->
    url_encode(T, << Acc/binary, $&, (escape_uri(Key))/binary
                   , $=, (escape_uri(Value))/binary >>).

escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S), <<>>);
escape_uri(B) ->
    escape_uri(B, <<>>).

escape_uri(<<C, Rest/binary>>, Acc) ->
    if  C >= $0, C =< $9 -> escape_uri(Rest, <<Acc/binary, C>>);
        C >= $A, C =< $Z -> escape_uri(Rest, <<Acc/binary, C>>);
        C >= $a, C =< $z -> escape_uri(Rest, <<Acc/binary, C>>);
        C =:= $          -> escape_uri(Rest, <<Acc/binary, $+>>);
        C =:= $.; C =:= $-; C =:= $~; C =:= $_ ->
            escape_uri(Rest, <<Acc/binary, C>>);
        true ->
            H = C band 16#F0 bsr 4, L = C band 16#0F,
            escape_uri(Rest, <<Acc/binary, $%, (tohexl(H)), (tohexl(L))>>)
    end;
escape_uri(<<>>, Acc) ->
    Acc.

tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 17 -> $a + C - 10.
