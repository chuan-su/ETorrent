%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%
%%% Created : 2011-11-8
%%% -------------------------------------------------------------------
-module(client_sup).

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).
-export([start_fsm/0,start_connection/4]).

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
  
	supervisor:start_link({local,client_sup}, ?MODULE, []).

start_fsm()->
	supervisor:start_child(fsm_connection_sup, []).

start_connection(Host,Port,Info_hash,My_peer_id)->
	
	supervisor:start_child(tcp_client_sup, [Host,Port,Info_hash,My_peer_id]).


%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	
	
	SupFlags ={one_for_one,?MAX_RESTART,?MAX_TIME},
	
	%% tcp_client_sup 
	ChildSpec_One= {tcp_client_sup,
					{tcp_client_sup,start_link,[]},
					permanent,
					infinity,
					supervisor,
					[tcp_client_sup]},
	%% fsm_connection_sup
	
	ChildSpec_Two= {fsm_connection_sup,
					{fsm_connection_sup,start_link,[]},
					 permanent,
					 infinity,
					 supervisor,
					[fsm_connection_sup]},
	
	
	ChildSpec_Three= {multiple_file_handler_sup,
					{multiple_file_handler_sup,start_link,[]},
					 permanent,
					 infinity,
					 supervisor,
					[multiple_file_handler_sup]},
	
	File = torrent_info:fetch_path(),
	case built_connections_fsm:file_types(File) of
		multiple_file ->
	      io:format("~s",["ok,start_multiple_file"]),
    {ok,{SupFlags,[ChildSpec_One,ChildSpec_Two,ChildSpec_Three]}};
		single_file ->
			io:format("~s",["ok,start_single_file"]),
       {ok,{SupFlags,[ChildSpec_One,ChildSpec_Two]}}
	end.
