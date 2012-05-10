%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Acceptor module, accepts incoming conections for
%%      already created socket.
-module(gen_tcpd_acceptor).
-created('Date: 27/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').

-define(ACCEPT_TIMEOUT, 1000).

-export([start_link/3]).
-export([bind/3]).

start_link(Socket, SockOptions, MFA) ->
	{ok, spawn_link(?MODULE, bind, [Socket, SockOptions, MFA])}.

bind(ListenSocket, SockOptions, MFA) ->
	case gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT) of
		{ok, Socket} ->
			{ok, Pid} = supervisor:start_child(gen_tcpd_conns_sup, [Socket, MFA]),
			gen_tcp:controlling_process(Socket, Pid),
			inet:setopts(Socket, SockOptions),
			bind(ListenSocket, SockOptions, MFA);
		{error, timeout} ->
			bind(ListenSocket, SockOptions, MFA)
	end.
