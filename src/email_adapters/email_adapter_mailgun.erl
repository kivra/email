%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2012-2014 Kivra
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc Email Mailgun Adapter
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(email_adapter_mailgun).
-behaviour(email_adapter).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([start/0]).
-export([start/1]).
-export([stop/1]).
-export([send/6, send/7]).
-export([url_encode/1]).

%%%_ * Types -----------------------------------------------------------
-record(state, {
        apiurl :: string(),
        apikey :: string()
        }).

%%%_* Macros ===========================================================
-define(http_options, [ {timeout,         120000}
                      , {connect_timeout, 10000}
                      , {version,         "HTTP/1.0"} ]).
-define(options, [{body_format, binary}, {full_result, false}]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
start()        -> start([]).
start(Options) ->
    application:start(inets),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    Domain = proplists:get_value(domain, Options),
    ApiUrl = proplists:get_value(apiurl, Options),
    ApiKey = proplists:get_value(apikey, Options),
    {ok, #state{apiurl=ApiUrl++"/"++Domain, apikey=ApiKey}}.

stop(_Conn)    -> ok.


%% Send directly without email controller.
send(ApiUrl, ApiKey, To, From, Subject, Message, Opt) ->
  send(#state{apiurl = ApiUrl, apikey = ApiKey}, To, From, Subject, Message, Opt).

send(Conn, {ToEmail, ToEmail}, {FromName, FromEmail}, Subject, Message, Opt) ->
    send(Conn, {<<>>, ToEmail}, {FromName, FromEmail}, Subject, Message, Opt);
send(Conn, {ToName, ToEmail}, {FromEmail, FromEmail}, Subject, Message, Opt) ->
    send(Conn, {ToName, ToEmail}, {<<>>, FromEmail}, Subject, Message, Opt);
send(Conn, To, {FromName, FromEmail}, Subject, Message, Opt) ->
    Body0 = [ {<<"to">>,      form_to(To)}
            , {<<"from">>,    <<FromName/binary, $<, FromEmail/binary, $>>>}
    ],
    Req   = construct_request(Conn, Subject,add_message( Message
                                               , lists:merge(Opt, Body0)
                                               )),

    try httpc:request(post, Req, ?http_options, ?options) of
        {ok, {200, Payload}} -> {ok, Payload};
        {ok, {_, Payload}}   -> {error, Payload};
        Error                -> Error
    catch
        exit:{timeout, _} -> {error, timeout}
    end.

%%%_* Private functions ================================================
add_message([], Body)    -> Body;
add_message([H|T], Body) -> add_message(T, [H|Body]).

construct_request(Conn, Subject,Body) ->
    { Conn#state.apiurl++"/"++"messages"
    , auth_header("api", Conn#state.apikey)
    , "application/x-www-form-urlencoded"
    , add_subject(url_encode(Body), Subject) }.

add_subject(Body, Subject) ->
  <<Body/binary, <<"&subject=">>/binary,
    <<"=?utf-8?B?">>/binary, (base64:encode(Subject))/binary, <<"?=">>/binary >>.

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
                   , $=,  (escape_uri(Value))/binary >>).

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

%% @private
form_to([{ToName, ToEmail}]) ->
  <<ToName/binary, $<, ToEmail/binary, $>>>;
form_to(ToList) when is_list(ToList) ->
  lists:foldl(
    fun
      (To, <<>>) ->
        form_to(To);
      (To, Acc) ->
        <<(form_to(To))/binary, <<",">>/binary, Acc/binary>>
    end, <<>>, ToList);
form_to({ToName, ToEmail}) ->
  <<ToName/binary, $<, ToEmail/binary, $>>>.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

form_to_single_test() ->
  To = form_to({<<"boss">>, <<"boss@gmail.com">>}),
  ?assertEqual(<<"boss<boss@gmail.com>">>, To).

form_to_one_in_list_test() ->
  To = form_to([{<<"boss">>, <<"boss@gmail.com">>}]),
  ?assertEqual(<<"boss<boss@gmail.com>">>, To).

form_to_many_in_list_test() ->
  To = form_to([{<<"boss">>, <<"boss@gmail.com">>}, {<<"cto">>, <<"cto@gmail.com">>}, {<<"ceo">>, <<"ceo@gmail.com">>}]),
  ?assertEqual(<<"ceo<ceo@gmail.com>,cto<cto@gmail.com>,boss<boss@gmail.com>">>, To).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
