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
%%% @doc Email API
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(email).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([start_link/1, start_link/0]).
-export([send/5]).
-export([send/6]).
-export([send/7]).

-export_type([email/0]).
-export_type([message/0]).
-export_type([options/0]).

%%%_* Macros ===========================================================
%% Make sure we time out internally before our clients time out.
-define(TIMEOUT, 120000). %gen_server:call/3

%%%_ * Types -----------------------------------------------------------
-type email()           :: {binary(), binary()}.
-type message_element() :: binary() | {html, binary()} | {text, binary()}.
-type message()         :: message_element() | list(message_element()).
-type option()          :: hibernate.
-type options()         :: [option()].
-type dirtyemailprim()  :: atom() | list() | binary().
-type dirtyemail()      :: {dirtyemailprim(), dirtyemailprim()}.
-type maybedirtyemail() :: email() | dirtyemail() | dirtyemailprim().

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> start_link([]).

-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, any()}.
start_link(Opts) -> email_controller:start_link(Opts).

%% @doc Sends an email and returns ok or error depending on the outcome
-spec send(pid(),maybedirtyemail(), maybedirtyemail(), binary(), message()) ->
            {ok, term()} | {error, term()}.
send(Controller, To, From, Subject, Message) ->
  send(Controller, To, From, Subject, Message, []).

-spec send(pid(),maybedirtyemail(), maybedirtyemail(), binary(), message(), options()) ->
  {ok, term()} | {error, term()}.
send(Controller, To, From, Subject, Message, Options) ->
  send(Controller, To, From, Subject, Message, Options, []).


%% @doc Sends an email and returns ok or error depending on the outcome
-spec send(pid(),maybedirtyemail(), maybedirtyemail(), binary(), message(), options(), any()) ->
            {ok, term()} | {error, term()}.
send(Controller, To, From, Subject, Message, Options, Params) ->
    gen_server:call( Controller, { send
                                      , sanitize_param(To)
                                      , sanitize_param(From)
                                      , ensure_binary(Subject)
                                      , sanitize_message(Message)
                                      , Options
                                      , Params}
                   , ?TIMEOUT).

%%%_* Private functions ================================================
-spec sanitize_param(maybedirtyemail()) -> email().
sanitize_param({V1, V2}) -> {ensure_binary(V1), ensure_binary(V2)};
sanitize_param(Val)      -> {ensure_binary(Val), ensure_binary(Val)}.

sanitize_message({html, Msg}) -> [{<<"html">>, ensure_binary(Msg)}];
sanitize_message({text, Msg}) -> [{<<"text">>, ensure_binary(Msg)}];
sanitize_message(Msg) when is_list(Msg) ->
    lists:foldl(fun(Element, Acc) ->
        case Element of
            {html, H} -> [{<<"html">>, ensure_binary(H)} | Acc];
            {text, H} -> [{<<"text">>, ensure_binary(H)} | Acc]
        end
    end, [], Msg);
sanitize_message(Msg)         -> [{<<"text">>, ensure_binary(Msg)}].

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L)   -> list_to_binary(L);
ensure_binary(A) when is_atom(A)   -> ensure_binary(atom_to_list(A)).