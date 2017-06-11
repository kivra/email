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
%%% @doc Email Mock Adapter
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(email_adapter_mock).
-behaviour(email_adapter_intf).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([start/0]).
-export([start/1]).
-export([stop/1]).
-export([send/6]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
start()         -> start([]).
start(_Options) -> {ok, []}.
stop(_)         -> ok.

send(_, {ToName, ToEmail}, {FromName, FromEmail}, Subject, Message, Opt) ->
    io:format("(~p, ~p)-(~p, ~p)~n~p~n~p~n~p"
             , [ToName, ToEmail, FromName, FromEmail, Subject, Message, Opt]),
    {ok, mock}.