%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Main supervisor for gen_tcpd application.
-module(gen_tcpd_sup).
-created('Date: 27/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	{ok, {{one_for_one, 5, 60}, [
		{gen_tcpd_pools_sup,
			{gen_tcpd_pools_sup, start_link, []},
			permanent,
			5000,
			worker,
			[gen_tcpd_pools_sup]},
		{gen_tcpd_conns_sup,
			{gen_tcpd_conns_sup, start_link, []},
			permanent,
			5000,
			worker,
			[gen_tcpd_conns_sup]}
	]}}.
