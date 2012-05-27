%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc gen_tcpd application.
%%      Makes easy to create TCP daemons.
%%      Should be started with `application:start(gen_tcpd)'.
-module(gen_tcpd).
-created('Date: 27/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').
-behaviour(application).

% application API
-export([start/2, stop/1]).
% API
-export([bind/2, bind/3, unbind/1,
	send/2, activate/1, activate/2, close/1]).

%% ---------------------------------------------------------------------
%% Public methods (открытые методы).
%% ---------------------------------------------------------------------

%% @doc Starts an application.
start(_Type, _Args) ->
	gen_tcpd_sup:start_link().

%% @doc Stops an application.
stop(_State) ->
	ok.

%% @doc Same as `bind/2' with empty `Options' list.
%% @see bind/3
bind(Port, MFA) ->
	bind(Port, MFA, []).

%% @doc Binds callback module to specified port.
%%      This method creates listen socket and distributes it on
%%      acceptors pool (default - 5 workers).
%%
%%      `MFA' will be called after succeeded connection on `Port'.
%%
%%      `MFA' must return `{ok, State}'.
%%
%%      `MFA' must have behaviour `gen_tcpd_handler_behaviour'.
%%
%%      See `gen_tcpd_handler_example_echo' to learn how to write simple
%%      callback module.
%%
%%      See `gen_tcpd_conn' to learn how to control socket connection.
%%
%%      `Options' is a prolist with keys:
%%      <ul>
%%        <li>`pool' describes acceptors pool options:
%%          <ul>
%%            <li>`workers' - workers count (default - 5);</li>
%%            <li>`max_restarts' - maximum restarts
%%                in `restarts_time' (default - 5);</li>
%%            <li>`restarts_time' - maximum interval
%%                in seconds for `max_restarts' (default - 60);</li>
%%          </ul>
%%        </li>
%%        <li>`socket' describes listen socket options:
%%          <ul>
%%            <li>all options from `inet:setopts()' except `active'.</li>
%%          </ul>
%%        </li>
%%      </ul>
%% @see gen_tcpd_handler_behaviour
%% @see gen_tcpd_handler_example_echo
-spec bind(integer(), mfa(), list()) -> {ok, pid()}.
bind(Port, MFA, Options) ->
	supervisor:start_child(gen_tcpd_pools_sup,
		[Port, MFA, Options]).

%% @doc Remove binding from `Port'.
-spec unbind(integer()) -> ok.
unbind(Port) ->
	supervisor:terminate_child(gen_tcpd_pools_sup,
		whereis(gen_tcpd_pools_sup:make_child_name(Port))).

%% @doc Sends `Data' to socket, linked to connection with pid `Pid'.
-spec send(pid(), list()) -> ok.
send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

%% @doc Switch socket to active state.
%%      `recv/2' from callback module will be callbed,
%%      when some data arrived at socket.
-spec activate(pid()) -> ok.
activate(Pid) ->
	activate(Pid, 0).

%% @doc Switches socket to active state in buffered mode.
%%      `recv/2' from callback module will be called only after
%%      `BufferSize' bytes was arrived at socket.
-spec activate(pid(), pos_integer()) -> ok.
activate(Pid, BufferSize) ->
	gen_server:cast(Pid, {activate, BufferSize}).

%% @doc Closes connection.
-spec close(pid()) -> ok.
close(Pid) ->
	gen_server:cast(Pid, close).
