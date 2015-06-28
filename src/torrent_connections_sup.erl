%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%
%%% Created : 2011-12-12
%%% -------------------------------------------------------------------
-module(torrent_connections_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

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

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start_link()->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).


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
	
     SupFlags ={one_for_one,?MAX_RESTART,?MAX_TIME},
	 
	 ChildSpec_One ={connections_manager_sup,
				 {connections_manager_sup,start_link,[]},
				  permanent,
				  infinity,
				  supervisor,
				 [connections_manager_sup]},
	
	  ChildSpec_Two ={peer_state_sup,
				 {peer_state_sup,start_link,[]},
				  permanent,
				  infinity,
				  supervisor,
				 [peer_state_sup]},
	  ChildSpec_Three ={torrent_request_sup,
				 {torrent_request_sup,start_link,[]},
				  permanent,
				  infinity,
				  supervisor,
				 [torrent_request_sup]},
	 
	   ChildSpec_Four= {client_sup,
					{client_sup,start_link,[]},
					 permanent,
					 infinity,
					 supervisor,
					[client_sup]},
	 
     {ok,{SupFlags,[ChildSpec_One,ChildSpec_Two,ChildSpec_Three,ChildSpec_Four]}}.


%% ====================================================================
%% Internal functions
%% ====================================================================

