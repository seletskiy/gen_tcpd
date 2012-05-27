%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Module implements 2 roles: recv and send.
%%      <ol>
%%        <li>it receives all messages, that are sended by connected client;</li>
%%        <li>it sends messages to connected client that are specified in {@link send/2}.</li>
%%      </ol>
%%
%%      Socket opens with `{active, false}' option, so you need manually to
%%      set it to be able to accept incoming messages. It can be done
%%      with `activate/1' and `wait_data/2' functions.
%%
%%      See `gen_tcpd_handler_example_echo' for example usage.
%%
%% @see activate/1
%% @see activate/2
%% @see gen_tcpd_handler_example_echo
-module(gen_tcpd_conn).

-behaviour(gen_server).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    sock :: gen_tcp:socket(),
    module :: module(),
    module_state :: any(),
	buffer_pid :: pid()}).

%% ---------------------------------------------------------------------
%% Public methods (открытые методы).
%% ---------------------------------------------------------------------

%% @doc Start connection module and spawn user defined `Module'.
-spec start_link(gen_tcp:socket(), mfa()) -> {ok, pid()}.
start_link(Socket, MFA) ->
    gen_server:start_link(?MODULE, [Socket, MFA], []).

%% ---------------------------------------------------------------------
%% Private methods (закрытые методы).
%% ---------------------------------------------------------------------

recv_data(Socket, 0) ->
	inet:setopts(Socket, [{active, once}]),
	undefined;

recv_data(Socket, BufferSize) ->
	{BufferPid, _} = gen_tcpd_conn_buffer:start_monitor(
		Socket, self(), BufferSize),
	BufferPid.

%% ---------------------------------------------------------------------
%% gen_server specific.
%% ---------------------------------------------------------------------

%% @private
init([Socket, _MFA = {Module, Function, Args}]) ->
	{ok, ModState} = apply(Module, Function, [self()] ++ Args),
    {ok, #state{
        sock = Socket,
        module = Module,
        module_state = ModState}}.

%% @private
handle_call(_Message, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({activate, BufferSize}, State = #state{buffer_pid = undefined}) ->
    #state{sock = Socket} = State,
	BufferPid = recv_data(Socket, BufferSize),
	{noreply, State#state{
		buffer_pid = BufferPid}};

%% @private
handle_cast({activate, _}, State) ->
	{noreply, State};

%% @private
handle_cast({send, Data}, State) ->
    #state{sock = Socket} = State,
    gen_tcp:send(Socket, Data),
    {noreply, State};

handle_cast(close, State) ->
	#state{sock = Socket} = State,
	gen_tcp:close(Socket),
    {stop, normal, State};

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info({tcp, _Socket, Data}, State) ->
    #state{
        module = Module,
        module_state = ModState} = State,
	NewModState = Module:recv(ModState, Data),
    {noreply, State#state{module_state = NewModState}};

%% @private
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

%% @private
handle_info({'DOWN', _, _, _, normal}, State) ->
	{noreply, State#state{buffer_pid = undefined}};

%% @private
handle_info({'DOWN', _, _, _, _}, State) ->
    {stop, normal, State};

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    #state{
        module = Module,
        module_state = ModState} = State,
    Module:stop(ModState),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
