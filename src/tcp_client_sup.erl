%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%             Simple one for one supervisor for starting each connetion process
%%% Created : 2011-11-14
%%% -------------------------------------------------------------------
-module(tcp_client_sup).

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
%% External functions
%% ====================================================================

start_link()->
	supervisor:start_link({local,tcp_client_sup}, ?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	Sup_Flags= {simple_one_for_one,?MAX_RESTART,?MAX_TIME},
	
    Child_Spec ={undefined,                                  %% Internerl ID
			    {tcp_client,start_link,[]},                  %% Start Function {M,F,A}
				 temporary,                                  %% Restart = permanent|transient|temporary
				 2000,                                       %% Shutdown = brutal_kill | int()>0|infinity
				 worker,                                     %% Type  = worker | supervisor
				[tcp_client]},                               %% Module = [Modules]|dynamic
	
						  
    {ok,{Sup_Flags,[Child_Spec]}}.


