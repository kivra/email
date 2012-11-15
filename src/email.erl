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
