%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Supervisor for listener processes.
-module(gen_tcpd_pools_sup).
-created('Date: 27/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([make_child_name/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	{ok, {{simple_one_for_one, 5, 60}, [
		{gen_tcpd_pool_sup,
			{gen_tcpd_pool_sup, start_link, []},
			transient,
			5000,
			worker,
			[gen_tcpd_pool_sup]}
	]}}.

make_child_name(Port) ->
	list_to_atom("gen_tcpd_pool_sup+" ++ integer_to_list(Port)).
