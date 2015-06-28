%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description : Connections manager , maintains the max connections,and update the peers list.
%%%
%%% Created : 2011-11-28
%%% -------------------------------------------------------------------
-module(connections_manager).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

-export([file_info/2,peer_to_peer/1]).
-export([new_peers/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ----------------------------------------------------------------------
%% Records
%% ----------------------------------------------------------------------
-record(state, {peers,
				info_hash ,
				peer_id}).

-define(CHECK_TIME,timer:seconds(120)).

%% ====================================================================
%% External functions
%% ====================================================================

%% Return new peer list without duplicated peers
new_peers([Head|Tail],Acc)->
	
	new_peers(Tail--[Head],[Head|Acc]);
new_peers([],Acc)->
	lists:reverse(Acc).

	
%% ----------------------------------------------------------------------------
%% Start to connect to peers according to the peer list from tracker
%% ----------------------------------------------------------------------------
start_connections(Peers,#state{peers = Available_peers,
							   info_hash = Info_hash,
							   peer_id =  Peer_id} = State) ->

%% Merge the new peer list and the remaining peer list
%% Return a peer list without duplicated peers.
	
	Merge_list = Peers++Available_peers,
	New_Peers = new_peers(Merge_list,[]), 
	
%% Check whether it reached the max connections	
	
	case peer_table:available_connections() of
		
		full_connections ->                           %% Reach the max connections.
			
			{noreply,State#state{peers = New_Peers}};
		
		Available when is_integer(Available) ->       %% We did not reached the max connections amount.
			
		      Remaining_peers = handle_max_connections(New_Peers,Available,Info_hash,Peer_id),
		
		 {noreply,State#state{peers = Remaining_peers}}
	end.
			
%% -------------------------------------------------------------------------
%% Traverse the whole peer list and try to connect each peer if available.
%% -------------------------------------------------------------------------

handle_max_connections([{Ip,Port}|Rest],Available,Info_hash,Peer_id)->
	
	case peer_table:is_connected(Ip, Port) of %% Check whether a peer is connected already.
		
		true ->                               %%  Return true means connected.
			
			handle_max_connections(Rest,Available,Info_hash,Peer_id); %% So try to connect next peer of the list
		false ->                              %%  Not connected the peer yet 
			
			 client_sup:start_connection(Ip, Port, Info_hash, Peer_id), %% Start the connetions with this peer.
			 
		  handle_max_connections(Rest,Available -1,Info_hash,Peer_id)  %% Traverse the peer list and change
	                                                                   %% the amount of available connections
	end;

%% Finish traversing the peer list and we did not reach the max 
%% connections amout , return [].
handle_max_connections([],_,_,_)->
	[];

%% Reach the max connections and available connections amount becomes Zero.
%% Return the peers left ,which we have not connected
handle_max_connections(Remaining,0,_,_)->
	Remaining.

           

%% ====================================================================
%% Server functions
%% ====================================================================
	
%% Start the connections manager server.
start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
%%  Inform the server the file info hash and peer id
%%  The server will maintain these file information and use it to connect peers
%% -----------------------------------------------------------------------------
file_info(Info_hash,Peer_id)->
	gen_server:cast(?MODULE, {file_info,Info_hash,Peer_id}).

%% -----------------------------------------------------------------------------
%% Receive the peer list from tracker and start the connections with the peer
%% Or receive the remaining peer list from the timer function
%% And start the connections
%-------------------------------------------------------------------------------

peer_to_peer(Peerlist)->
	gen_server:cast(?MODULE, {connection_start,Peerlist}).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}       
%% Start a timer and when the check time reached,connect the peers automaticly
%% --------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit,true),
	%%erlang:start_timer(?CHECK_TIME, ?MODULE,check_available_peers),
	erlang:send_after(?CHECK_TIME, self(), check_available_peers),
    {ok, #state{peers = []}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Receive the start connection request and start connect to peers
%% --------------------------------------------------------------------
handle_cast({connection_start,Peer_list}, State) ->
	
	New_state = start_connections(Peer_list,State),
	
    New_state;

%% ---------------------------------------------------------------------
%% Recive the file info request and store the file info hash and peer id
%% in our initial state 
%% ---------------------------------------------------------------------
handle_cast({file_info,Info_hash,Peer_id},State)->
	
	io:format("~p", [Info_hash]),

	 My_id  = list_to_binary(Peer_id),
	 
	{noreply,State#state{info_hash = <<Info_hash:20/binary>>,
						 peer_id   = <<My_id:20/binary>>}}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
	
%% ---------------------------------------------------------------------
%% As we start a timer for automatical connections ,when the time reached
%% Receive the {timeout,TimeRef,Message} message and try to connect the 
%% the peers.
%% ----------------------------------------------------------------------

handle_info(check_available_peers, State) ->
	
	gen_server:cast(?MODULE,{connection_start,[]}),
	erlang:send_after(?CHECK_TIME, self(), check_available_peers),
	
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

