%% @author Stanislav Seletskiy <s.seletskiy@office.ngs.ru>
%% @doc Behaviour for callback module of gen_tcpd.
%%      Behaviour ensures two methods:
%%      <ul>
%%        <li>`recv(ModulePid, Data)' will be called, when data
%%             was received from socket.</li>
%%            `ModulePid' is a pid, returned from `MFA' function in `gen_tcpd:bind()' call.
%%        <li>`stop(ModulePid)' will be called, when socket closes
%%             or on system shutdown</li>
%%      </ul>
-module(gen_tcpd_handler_behaviour).
-created('Date: 28/04/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@office.ngs.ru>').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{recv, 2}, {stop, 1}];

behaviour_info(_) ->
	undefined.
