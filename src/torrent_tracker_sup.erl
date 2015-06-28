%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%
%%% Created : 2011-11-30
%%% -------------------------------------------------------------------
-module(torrent_tracker_sup).

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
	 
	 ChildSpec_One ={torrent_download_records,
				 {torrent_download_records,start_link,[]},
				  permanent,
				  ?SHUT_DOWN,
				  worker,
				 [torrent_download_records]},
	
	 
	 ChildSpec_Two ={torrent_tracker,
				 {torrent_tracker,start_link,[]},
				  permanent,
				  ?SHUT_DOWN,
				  worker,
				 [torrent_tracker]},
	 
	 
	 
     {ok,{SupFlags,[ChildSpec_One,ChildSpec_Two]}}.



