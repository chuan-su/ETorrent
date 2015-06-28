%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%
%%% Created : 2011-12-12
%%% -------------------------------------------------------------------
-module(torrent_pieces_sup).

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
-define(SHUT_DOWN,5000).


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
	 
	 ChildSpec_One ={torrent_piece_handler,
				 {torrent_piece_handler,start_link,[]},
				  permanent,
				  ?SHUT_DOWN,
				  worker,
				 [torrent_piece_handler]},
	
	 ChildSpec_Two ={torrent_piece_table,
				 {torrent_piece_table,start_link,[]},
				  permanent,
				  ?SHUT_DOWN,
				  worker,
				 [torrent_piece_table]},
	 
	 ChildSpec_Three ={torrent_chunk_handler,
				 {torrent_chunk_handler,start_link,[]},
				  permanent,
				  ?SHUT_DOWN,
				  worker,
				 [torrent_chunk_handler]},
	 
     {ok,{SupFlags,[ChildSpec_One,ChildSpec_Two,ChildSpec_Three]}}.
%% ====================================================================
%% Internal functions
%% ====================================================================

