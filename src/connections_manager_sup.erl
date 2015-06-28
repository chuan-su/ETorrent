%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%
%%% Created : 2011-11-30
%%% -------------------------------------------------------------------
-module(connections_manager_sup).

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
-define(SHUT_DOWN,5000).


%% ====================================================================
%% Server functions
%% ====================================================================
start_link()->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).


%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	
     SupFlags ={one_for_one,?MAX_RESTART,?MAX_TIME},
	 
	 
	 ChildSpec_One ={peer_table,
				 {peer_table,start_link,[]},
				  permanent,
				  ?SHUT_DOWN,
				  worker,
				 [peer_table]},
	
	 ChildSpec_Two ={connections_manager,
				 {connections_manager,start_link,[]},
				  permanent,
				  ?SHUT_DOWN,
				  worker,
				 [connections_manager]},
	 
     {ok,{SupFlags,[ChildSpec_One,ChildSpec_Two]}}.
