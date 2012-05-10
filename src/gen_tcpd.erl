%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc gen_tcpd application.
%%      Makes easy to create TCP daemons.
%%      Should be started with `application:start(gen_tcpd)'.
-module(gen_tcpd).
-created('Date: 27/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').
-behaviour(application).

-export([start/2, stop/1]).
-export([bind/2, bind/3, unbind/1]).

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
%%        <li>`listener' describes listen socket options:
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
