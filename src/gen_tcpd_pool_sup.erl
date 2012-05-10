%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Supervisor for acceptors pool (one port).
-module(gen_tcpd_pool_sup).
-created('Date: 27/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').

-define(DEFAULT_MAX_R, 5).
-define(DEFAULT_MAX_T, 60).
-define(DEFAULT_WORKERS, 5).

-behaviour(supervisor).

-export([start_link/3, init/1]).

%% ---------------------------------------------------------------------
%% Public methods (открытые методы).
%% ---------------------------------------------------------------------
start_link(Port, MFA, Options) ->
	supervisor:start_link(
		{local, gen_tcpd_pools_sup:make_child_name(Port)},
		?MODULE, [Port, MFA, Options]).

init([Port, MFA, Options]) ->
	PoolOptions = proplists:get_value(pool, Options, []),
	ListenerOptions = proplists:get_value(listener, Options, []),
	MaxRestarts = proplists:get_value(max_restarts, PoolOptions, ?DEFAULT_MAX_R),
	RestartsTime = proplists:get_value(restarts_time, PoolOptions, ?DEFAULT_MAX_T),
	WorkersCount = proplists:get_value(workers, PoolOptions, ?DEFAULT_WORKERS),
	Socket = gen_tcpd_listener:listen(Port, ListenerOptions),
	{ok, {{one_for_one, MaxRestarts, RestartsTime},
		lists:map(fun(Index) ->
			create_child_spec(Index, [Socket, ListenerOptions, MFA]) end,
			lists:seq(1, WorkersCount))
	}}.


%% ---------------------------------------------------------------------
%% Private methods (закрытые методы).
%% ---------------------------------------------------------------------
create_child_spec(Index, Args) ->
	{list_to_atom("gen_tcpd_listener+" ++ integer_to_list(Index)),
		{gen_tcpd_acceptor, start_link, Args},
		permanent,
		5000,
		worker,
		[gen_tcpd_listener]}.
