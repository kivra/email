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
    ApiUrl = proplists:get_value(apiurl, Options),
    ApiKey = proplists:get_value(apikey, Options),
    {ok, #state{apiurl=ApiUrl++"/"++Domain, apikey=ApiKey}}.

stop(_Conn) ->
    ok.

send(Conn, {ToName, ToEmail}, {FromName, FromEmail}, Subject, Message) ->
    Body = [{<<"to">>, <<ToName/binary, " <", ToEmail/binary, ">">>},
            {<<"from">>, <<FromName/binary, " <", FromEmail/binary, ">">>},
            {<<"html">>, Message},
            {<<"subject">>, Subject}],

    case httpc:request( post, construct_request(Conn, Body)
                      , [], [{body_format, binary}] ) of
        {ok, {{_, 200, _}, _, Payload}} -> {ok, Payload};
        {ok, {{_, _, _}, _, Payload}}   -> {error, Payload};
        Error                           -> Error
    end.


%%% Private ========================================================================


construct_request(Conn, Body) ->
    { Conn#state.apiurl++"/"++"messages"
    , auth_header("api", Conn#state.apikey)
    , "application/x-www-form-urlencoded"
    , url_encode(Body) }.

auth_header(User, Password) ->
    [{ "Authorization"
     , "Basic "++base64:encode_to_string(lists:append([User,":",Password])) }].

url_encode(Data)    -> url_encode(Data,"").

url_encode([], Acc) -> Acc;
url_encode([{Key, Value} | T], "") ->
    url_encode(T, escape_uri(Key)++"="++escape_uri(Value));
url_encode([{Key, Value} | T], Acc) ->
    url_encode(T, Acc++"&"++escape_uri(Key)++"="++escape_uri(Value)).

escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].
