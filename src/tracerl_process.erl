%%%-------------------------------------------------------------------
%%% @author Paweł Chrząszcz <pawel.chrzaszcz@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 11 Jul 2013 by <pawel.chrzaszcz@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(tracerl_process).

-behaviour(gen_server).

%% API
-export([start_link/4, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port, quit_pid, src_file, handler, script, data=[]}).

-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ScriptSrc, Node, Handler, Options) ->
    F = case lists:member(term, Options) of
            true  -> tracerl_util:term_handler(Handler);
            false -> Handler
        end,
    Args = [ScriptSrc, Node, F],
    case lists:keyfind(name, 1, Options) of
        {name, NameSpec} ->
            gen_server:start_link(NameSpec, ?MODULE, Args, []);
        false ->
            gen_server:start_link(?MODULE, Args, [])
    end.

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ScriptSrc, Node, Handler]) ->
    {Port, QuitPid, SrcFile, Script} =
        tracerl_util:start_trace(ScriptSrc, Node),
    {ok, #state{port = Port, quit_pid = QuitPid,
                src_file = SrcFile, handler = Handler,
                script = Script}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_script, _From, State = #state{script = Script}) ->
    {reply, Script, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State = #state{quit_pid = QuitPid}) ->
    tracerl_util:quit(QuitPid),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {data, {noeol, LinePart}}},
            State = #state{port = Port, data = Data}) ->
    {noreply, State#state{data = [LinePart|Data]}};
handle_info({Port, {data, {eol, Line}}},
            State = #state{port = Port, handler = Handler, data = Data}) ->
    Handler({line, lists:flatten(lists:reverse([Line|Data]))}),
    {noreply, State#state{data=[]}};
handle_info({Port, eof},
            State = #state{port = Port, handler = Handler, data = Data}) ->
    case Data of
        [] -> ok;
        _  -> Handler({line, lists:flatten(lists:reverse(Data))})
    end,
    Handler(eof),
    port_close(Port),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{src_file = SrcFile}) ->
    file:delete(SrcFile),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
