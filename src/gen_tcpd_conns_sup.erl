%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Supervisor for connections.
-module(gen_tcpd_conns_sup).
-created('Date: 27/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	{ok, {{simple_one_for_one, 5, 60}, [
		{gen_tcpd_conn,
			{gen_tcpd_conn, start_link, []},
			transient,
			5000,
			worker,
			[gen_tcpd_conn]}
	]}}.
