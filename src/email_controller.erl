-module(email_controller).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        adapter    :: atom(),
        connection :: term()
        }).

-define(DEFAULT_ADAPTER, mailgun).


%%% API ========================================================================


start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    AdapterName = get_app_env(adapter, ?DEFAULT_ADAPTER),
    AdapterOptions = get_app_env(?DEFAULT_ADAPTER, []),
    Adapter = list_to_atom(lists:concat(["email_adapter_", AdapterName])),
    {ok, Conn} = Adapter:start(AdapterOptions),
    {ok, #state{ adapter = Adapter, connection = Conn }}.

handle_call({send, To, From, Subject, Message}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:send(Conn, To, From, Subject, Message), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    Adapter:stop(Conn).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

get_app_env(Key, Default) ->
    case application:get_env(email, Key) of
        {ok, Val} -> Val;
        _         -> Default
    end.

