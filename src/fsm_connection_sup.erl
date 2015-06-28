%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%             Simple one for one supervisor for spawn fsm process for each connected peer
%%% Created : 2011-11-8
%%% -------------------------------------------------------------------
-module(fsm_connection_sup).

-behaviour(supervisor).


%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(MAX_RESTART,5).
-define(MAX_TIME,60).

%% ====================================================================
%% Server functions
%% ====================================================================

start_link()->
	
	supervisor:start_link({local,fsm_connection_sup}, ?MODULE,[]).
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	
	Sup_Flags= {simple_one_for_one,?MAX_RESTART,?MAX_TIME},
	
	%% simple_one_for_one,supervisor for fsm socket handler
	
	Child_Spec ={undefined,                                     %% Internerl ID
			    {built_connections_fsm,start_link,[]},          %% Start Function {M,F,A}
				 temporary,                                     %% Restart = permanent|transient|temporary
				 2000,                                          %% Shutdown = brutal_kill | int()>0|infinity
				 worker,                                        %% Type  = worker | supervisor
				 [built_connections_fsm,torrent_message]},      %% Module = [Modules]|dynamic
	
						  
    {ok,{Sup_Flags,[Child_Spec]}}.


