%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Buffer module for buffered read mode (by packet).
-module(gen_tcpd_conn_buffer).
-created('Date: 02/05/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').

-export([start_monitor/3, read/3]).

%% ---------------------------------------------------------------------
%% Public methods (открытые методы).
%% ---------------------------------------------------------------------

-spec start_monitor(port(), pid(), integer()) -> {ok, pid()}.
start_monitor(Socket, ConnPid, Size) ->
	spawn_monitor(?MODULE, read, [Socket, ConnPid, Size]).

%% ---------------------------------------------------------------------
%% Private methods (закрытые методы).
%% ---------------------------------------------------------------------

%% @private
read(Socket, ConnPid, Size) ->
	{ok, [{packet, Packet}]} = inet:getopts(Socket, [packet]),
	ok = inet:setopts(Socket, [{packet, raw}]),
	case gen_tcp:recv(Socket, Size) of
		{ok, Data} ->
			ok = inet:setopts(Socket, [{packet, Packet}]),
			ConnPid ! {tcp, Socket, Data};
		{error, closed} ->
			ConnPid ! {tcp_closed, Socket}
	end.
