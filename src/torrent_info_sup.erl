%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%
%%% Created : 2011-11-29
%%% -------------------------------------------------------------------
-module(torrent_info_sup).

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1]).

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

start_link(FileName) ->
	
	supervisor:start_link({local,?MODULE}, ?MODULE, FileName).
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(FileName) ->
    SupFlags ={one_for_one,?MAX_RESTART,?MAX_TIME},
	
	Child_Spec ={torrent_info,
				 {torrent_info,start_link,[FileName]},
				  permanent,
				  ?SHUT_DOWN,
				  worker,
				 [torrent_info,filverk]},
	
     {ok,{SupFlags,[Child_Spec]}}.



