%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Listener module, creates listener socket.
%%      Does not accept incoming connections.
-module(gen_tcpd_listener).
-created('Date: 27/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').

-export([listen/2]).

listen(Port, SockOptions) ->
	{ok, Socket} = gen_tcp:listen(Port, [{active, false}] ++ SockOptions),
	Socket.

