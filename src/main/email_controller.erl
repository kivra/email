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
%%% @doc Email Controller, responsible to keep state for the various adapters
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(email_controller).
-behaviour(gen_server).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%%_* Macros ===========================================================
-define(DEFAULT_ADAPTER, mock).

%%%_ * Types -----------------------------------------------------------
-record(state, {
        adapter    :: atom(),
        connection :: term()
        }).

%%%_* Code =============================================================
start_link(Options) -> gen_server:start_link(?MODULE, Options, []).

init(Options) ->
    AdpName    = proplists:get_value(adapter, Options, ?DEFAULT_ADAPTER),
    AdpOptions = proplists:get_value(AdpName, Options, []),
    Adapter    = list_to_atom(lists:concat(["email_adapter_", AdpName])),
    {ok, Conn} = Adapter:start(AdpOptions),
    {ok, #state{ adapter = Adapter, connection = Conn }}.

handle_call({send, To, From, Subject, Message, Options, Params}, _From, State) ->
  Res = (State#state.adapter):send( State#state.connection
    , To
    , From
    , Subject
    , Message
    , Params
  ),
  process_params(Res, State, Options).

handle_cast(_Request, State)        -> {noreply, State}.
terminate(_Reason, State)           -> (State#state.adapter):stop(
                                                    State#state.connection).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info(_Info, State)           -> {noreply, State}.


%% @private
process_params(Res, State, Params) ->
  case lists:member(hibernate, Params) of
    true -> {reply, Res, State, hibernate};
    false -> {reply, Res, State}
  end.