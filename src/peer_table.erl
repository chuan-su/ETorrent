%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description : A database maintains each connected peers, and delete the peer
%%%               if a connections drops ,and maintains the current connection amount and so on
%%%
%%% Created : 2011-11-28
%%% -------------------------------------------------------------------
-module(peer_table).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([available_connections/0,add_peer/4,is_connected/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).

%% ----------------------------------------------------------------------
%% Records
%% ----------------------------------------------------------------------
-record(peer,{pid ::pid(),
			  ip::{byte(),byte(),byte(),byte(),byte()},
			  port,
			  info_hash}).

-record(state,  {peer_monitors ::dict:new(),
				 max_connections :: integer(),
				 current_connections :: integer(),
				 remaining_connections :: integer()}).

%% ====================================================================
%% External functions
%% ====================================================================

%% Request how many available connections left.
available_connections()->
	case gen_server:call(?MODULE,available_connections) of
		Integer when is_integer(Integer) ->
			Integer;
		_ ->
			full_connections
	end.

%% Send request for adding the connected peer.

add_peer(Pid,Ip,Port,Info_hash)->
	Peer = #peer{pid = Pid,
				 ip  = Ip,
				 port = Port,
				 info_hash = Info_hash},
	gen_server:cast(?MODULE, {add_peers,Peer}).

%% Send request for checking whether the {IP,PORT} has connected.
is_connected(Ip,Port)->
	Reply = gen_server:call(?MODULE, {check_connected,Ip,Port}),
	Reply.

%% ====================================================================
%% Server functions
%% ====================================================================
%% Start rhe server.
start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% Create a table for connected peers and key is the Pid of the process
%% Whick connect the peer.
%% Maitains a state for connections amount ,we allowed the Maximum is 20.
%% --------------------------------------------------------------------
init([]) ->
	
	process_flag(trap_exit,true),

	ets:new(peers, [named_table,{keypos,#peer.pid},public]),
 
	State = #state{peer_monitors = dict:new(),
				   max_connections = 20,
				   current_connections = 0,
				   remaining_connections = 20},
    {ok,State}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% Check whether the peer has already connected.
%% --------------------------------------------------------------------
handle_call({check_connected,Ip,Port}, From, State) ->
	
	case ets:match(peers, #peer{pid = '_',ip = Ip,port = Port,info_hash = '_'}) of
		[]->
			{reply,false,State};
		List when is_list(List) ->
			{reply,true,State}
	end;
%% -------------------------------------------------------------------------------
%% Check how many connections amout are available ,if full return full_connection
%% --------------------------------------------------------------------------------

handle_call(available_connections,From,#state{remaining_connections =Available}=State) ->
	
	case Available >= 0 of 
		true ->
			{reply,Available,State};
		false ->
			{reply,full_connections,State}
	end;
%% Not expected request.
handle_call(_Msg,_From,State)->
	{reply,ok,State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Once a peer connected , will add this peer to the table
%% In the meantime ,start a monitor for that peer.
%% --------------------------------------------------------------------
handle_cast({add_peers,#peer{pid = Pid}= Peer},#state{peer_monitors = Monitor,
													  current_connections = Current,
													  remaining_connections = Remain} = State) ->
	
	ets:insert(peers,Peer),             %% Add the connected peer to table and key is 
	                                    %% the connected process pid.
	Ref = erlang:monitor(process, Pid), %% Start erlang minitor for that process.
	
    {noreply, State#state{peer_monitors = dict:store(Ref, Pid, Monitor),
						  current_connections = Current+1,   %% Updata the state
						  remaining_connections = Remain-1}};

%% Not a expected request.
handle_cast(_Msg,State)->
	{noreply,State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% As the server minitors each connected peers,so when a peer drop the 
%% the connection , The server receive the {'DOWN',Ref,Type,...} message
%% And the server will delete that peer from table.
%% Update the server state at last.
%% --------------------------------------------------------------------
handle_info({'DOWN',Ref,process,_,_},#state{peer_monitors = Monitor,
												  current_connections = Current,
												  remaining_connections = Remain}=State) ->
	
	case dict:find(Ref,Monitor) of %% Check wheher it is in out monitor dictionary.
		{ok,Pid} ->
			ets:delete(peers, Pid),
			
		{noreply,State#state{peer_monitors = dict:erase(Ref,Monitor), %% Erase the dictionary.
							 current_connections = Current-1,         %% Update the state.
							 remaining_connections = Remain+1}};
        error ->
             {noreply, State#state{peer_monitors = dict:erase(Ref,Monitor)}}
	end;

%% Nor expected message.
handle_info(_Msg,State)->
	{noreply,State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% Delete the table when shutdown.
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ets:delete(peers).

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

