%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%
%%% Created : 2011-11-30
%%% -------------------------------------------------------------------
-module(torrent_sup).

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1,start_torrent/0]).

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
%% Start the bit torrent client
%% ====================================================================
start_torrent()->
	torrent_tracker:start_connection().


%% ====================================================================
%% Server functions
%% ====================================================================
start_link(File)->
	
	supervisor:start_link({local,?MODULE}, ?MODULE, File).

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(File) ->
	
   SupFlags ={one_for_one,?MAX_RESTART,?MAX_TIME},
	
	%% torrent_info_sup 
	ChildSpec_One= {torrent_info_sup,
					{torrent_info_sup,start_link,[File]},
					permanent,
					infinity,
					supervisor,
					[torrent_info_sup]},
	%% torrent_tracker_sup
    ChildSpec_Two= {torrent_pieces_sup,
					{torrent_pieces_sup,start_link,[]},
					permanent,
					infinity,
					supervisor,
					[torrent_pieces_sup]},
	
	ChildSpec_Three= {torrent_tracker_sup,
					{torrent_tracker_sup,start_link,[]},
					 permanent,
					 infinity,
					 supervisor,
					[torrent_tracker_sup]},
	
    %% peer_table_sup 
	ChildSpec_Four= {torrent_connections_sup,
					 {torrent_connections_sup,start_link,[]},
					  permanent,
					  infinity,
					  supervisor,
					  [torrent_connections_sup]},

   
    {ok,{SupFlags,
		[ChildSpec_One,ChildSpec_Two,ChildSpec_Three,ChildSpec_Four]}}.

