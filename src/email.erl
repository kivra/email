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

-module(email).

-export([send/4]).
-export([send/5]).

-type email()           :: {binary(), binary()}.
-type message()         :: binary() | {html, binary()} | {text, binary()}.
-type dirtyemailprim()  :: atom() | list() | binary().
-type dirtyemail()      :: {dirtyemailprim(), dirtyemailprim()}.
-type maybedirtyemail() :: email() | dirtyemail() | dirtyemailprim().

-export_type([email/0]).
-export_type([message/0]).


%%% API ========================================================================


%% @doc Sends an email and returns ok or error depending on the outcome
-spec send(maybedirtyemail(), maybedirtyemail(), binary(), message()) ->
            {ok, term()} | {error, term()}.
send(To, From, Subject, Message) ->
    send(To, From, Subject, Message, []).

%% @doc Sends an email and returns ok or error depending on the outcome
-spec send(maybedirtyemail(), maybedirtyemail(), binary(), message(), any()) ->
            {ok, term()} | {error, term()}.
send(To, From, Subject, Message, Options) ->
    gen_server:call(email_controller, { send
                                      , sanitize_param(To)
                                      , sanitize_param(From)
                                      , ensure_binary(Subject)
                                      , sanitize_message(Message)
                                      , Options }
                    , infinity).


%%% Private ========================================================================


-spec sanitize_param(maybedirtyemail()) -> email().
sanitize_param({V1, V2}) ->
    {ensure_binary(V1), ensure_binary(V2)};
sanitize_param(Val) ->
    Val2 = ensure_binary(Val),
    {Val2, Val2}.

sanitize_message({html, Msg}) ->
    {html, ensure_binary(Msg)};
sanitize_message({text, Msg}) ->
    {text, ensure_binary(Msg)};
sanitize_message(Msg) ->
    ensure_binary(Msg).

ensure_binary(B) when is_binary(B) ->
    B;
ensure_binary(L) when is_list(L) ->
    list_to_binary(L);
ensure_binary(A) when is_atom(A) ->
    ensure_binary(atom_to_list(A)).
