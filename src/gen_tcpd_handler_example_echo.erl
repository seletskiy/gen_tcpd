%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Example echo client for gen_tcpd.
%%      Module starts with `start/1`.
%%
%%      To start module from REPL (`12345' is an example port):
%%
%%      <code>
%%        gen_tcpd_handler_example_echo:start(12345).
%%      </code>
-module(gen_tcpd_handler_example_echo).
-created('Date: 28/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').

-behaviour(gen_tcpd_handler_behaviour).

-export([start/1, init/1, recv/2, stop/1]).

start(Port) ->
	gen_tcpd:bind(
		Port, {?MODULE, init, []},
		[{socket, [
			{active, true},
			{reuseaddr, true}]}]).

%% @doc Starts module.
%%
%%      `ConnPid' is a pid of `gen_tcpd_conn' instance.
%%
%% @see gen_tcpd_conn
-spec init(pid()) -> {ok, pid()}.
init(ConnPid) ->
	{ok, ConnPid}.

%% @doc Will be called when some data received by socket.
-spec recv(pid(), list()) -> {pid()}.
recv(State = ConnPid, Data) ->
	gen_tcpd:send(ConnPid, Data),
	State.

%% @doc Will be called on socket close or system shutdown.
-spec stop(any()) -> ok.
stop(_State) ->
	ok.
