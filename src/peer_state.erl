%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description : Maintains an ets table storing peer state for each peer.
%%%
%%% Created : 2011-12-5
%%% -------------------------------------------------------------------
-module(peer_state).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,maintain_peer_pieces/1,update_peer_pieces/1,am_interested/0,
		 peer_chock_state/1,get_peer_state/0,get_remote_pieces/0,get_download_pieces/0,is_seeder/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

  
-record(state,  {pid,
                 pieces = [],
                 am_interested = false, %% connection start as chocked, not interested
				 peer_choking = true,
				 seeder = false
				 }).
-record(monitors,{peer_monitors ::dict:new()}).

%% ====================================================================
%% External functions
%% ====================================================================

%% When a valid bitfield message received ,these functions convert the 
%% bitfield to piece index list, so we can know what pieces the peer have.
peer_pieces(Pieces)->
	List = bitfield(Pieces,[]),
	Indexes = piece_index(List,0),
	Indexes.

bitfield(<<Byte:8,Rest/binary>>,Acc) ->
	
	Index = bitfield:convert(Byte),
	bitfield(Rest,Acc++Index);

bitfield(<<>>,Acc) ->
	Acc.

piece_index([Head|Tail],Index) ->
	if
		Head == 1 ->
			[Index|piece_index(Tail,Index+1)];
		true ->
			piece_index(Tail,Index+1)
	end;
piece_index([],_)->
	[].

handle_record(#state{pieces = Pieces}) ->
	Pieces.

%% Update the peer pieces list when receive "have" message.
new_pieces(Index,Pieces)->
	case lists:member(Index, Pieces) of
		true -> Pieces;
		false -> lists:sort([Index]++Pieces)
	end.

%% Intersect the pieces peer have and the pieces we have
%% return a pieces list that we can download from that peer.
available_download_pieces(Local,Remote)->
	Fun = fun(E,Acc) ->
				case  lists:member(E, Remote) of
					true -> [E|Acc];
					false -> Acc
				end end,
	Available_download_pieces = lists:foldl(Fun, [], Local),
	Available_download_pieces.
	
%%Check if a peer have all the pieces
check_seeder(List) ->
	Size = torrent_piece_handler:piece_amount(),
	Seeder = lists:seq(0, Size-1),
	case Seeder -- List of
		[] -> true;
		L  when is_list(L) -> false
	end.
		
%% ====================================================================
%% Server functions
%% ====================================================================
start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

maintain_peer_pieces(Pieces) ->
	gen_server:call(?MODULE, {peer_pieces,Pieces}).

update_peer_pieces(Piece) ->
	gen_server:call(?MODULE,{peer_have,Piece}).

am_interested()->
	gen_server:call(?MODULE,{am_interested,true}).

peer_chock_state(Choke_State) ->
	gen_server:call(?MODULE,{peer_choking,Choke_State}).

get_peer_state() ->
	gen_server:call(?MODULE,get_peer_state).

get_remote_pieces() ->
	gen_server:call(?MODULE,get_remote_pieces).

get_download_pieces() ->
	gen_server:call(?MODULE, available_download_pieces).

is_seeder()->
	gen_server:cal(?MODULE,check_seeder).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    %% start a table to store peer state for each peer, the key is the pid of that process
	ets:new(peer_state, [public,named_table,{keypos,#state.pid}]),
	
    {ok,#monitors{peer_monitors = dict:new()}}. %% start a monitor

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% ---------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Maitain the pieces the peer have.
%% ----------------------------------------------------------------

handle_call({peer_pieces,Pieces}, {Pid,_}, #monitors{peer_monitors = Monitor} = State) ->
	
	Peer_pieces = peer_pieces(Pieces),
	case ets:lookup(peer_state, Pid) of 
		[] -> 			  
	          Peer_state = #state{pid = Pid,pieces = Peer_pieces},
	          ets:insert(peer_state, Peer_state),
			  Ref = erlang:monitor(process, Pid); %% Start erlang minitor for that process.
			 %% io:format("~s~n", ["ok,pieces successfully inserted"]);
		[Peer_state] ->
			  ets:insert(peer_state,Peer_state#state{pieces = Peer_pieces}),
			  Ref = erlang:monitor(process, Pid) %% Start erlang minitor for that process.
	end,		  
    Reply = ok,
    {reply, Reply, State#monitors{peer_monitors = dict:store(Ref, Pid, Monitor)}};

%% ------------------------------------------------------------------------
%% Update the pieces the peer have
%% ------------------------------------------------------------------------
handle_call({peer_have,Index},{Pid,_},State) ->
	
     case ets:lookup(peer_state, Pid) of
		 [Peer_state] ->
			 Peer_pieces = handle_record(Peer_state),
			 New_pieces = new_pieces(Index,Peer_pieces),
			 ets:insert(peer_state,#state{pieces = New_pieces});
			%% io:format("~s~n", ["ok,piece index inserted"]);
		 [] ->
			 io:format("~s~n", ["have message fist arrived"])
	 end,
	 Reply = ok,
	 {reply,Reply,State};

%% ---------------------------------------------------------------------------
%% Update "am_interested" for the peer state
%% ---------------------------------------------------------------------------

handle_call({am_interested,true},{Pid,_},State) ->
	
	case ets:lookup(peer_state,Pid) of 
		[Peer_state] ->
			ets:insert(peer_state,Peer_state#state{am_interested = true})
	end,
	Reply = ok,
	{reply,Reply,State};

%% ---------------------------------------------------------------------------
%% Update "am_choking"  to unchoke for the peer state
%% ---------------------------------------------------------------------------
handle_call({peer_choking,unchoke},{Pid,_},State) ->
	
	case ets:lookup(peer_state,Pid) of 
		 [Peer_state] ->
			 ets:insert(peer_state,Peer_state#state{peer_choking = false});
		[]-> some_how
	
	end,
	Reply = ok,
	{reply,Reply,State};

%% ---------------------------------------------------------------------------
%% Update "am_choking"  to choke for the peer state
%% ---------------------------------------------------------------------------
handle_call({peer_choking,choke},_From,State) ->
	
	Reply = ok,
	{reply,Reply,State};

%% ---------------------------------------------------------------------------
%% get the peer state for specified peer and return allow_download or
%% not_allowed.
%% ---------------------------------------------------------------------------
handle_call(get_peer_state,{Pid,_},State) ->
	
    Reply =  case ets:lookup(peer_state,Pid) of 
		       [#state{am_interested = Interested,peer_choking = Chock}] ->
		       if 
			     Interested == true andalso Chock == false ->
				 {ok,allow_download};
			     true ->
				 {no,not_allowed}
		       end;
			   [] -> no_peer_state
	         end,
   {reply,Reply,State};
%% -----------------------------------------------------------------------------
%% get the pieces the peer have
%% ------------------------------------------------------------------------------
handle_call(get_remote_pieces,{Pid,_},State) ->
	
	Reply = case ets:lookup(peer_state,Pid) of
				[#state{pieces = Remote_pieces}] ->
					{ok,Remote_pieces};
				[] ->
					{error,no_pieces_available}
			end,
	{reply,Reply,State};


%% --------------------------------------------------------------------------------
%%  get available download pieces for this peer
%% --------------------------------------------------------------------------------
handle_call(available_download_pieces,{Pid,_},State)->
	     
	 Reply = case ets:lookup(peer_state,Pid) of 
				 [#state{pieces = Remote_pieces}] ->
					 Local = torrent_piece_table:get_need_pieces(),
					 available_download_pieces(Local,Remote_pieces);
				 []->
					 {error,no_such_peer}
			 end,
	 {reply,Reply,State};
					 
%% ---------------------------------------------------------------------------------
%% check if this peer is a seeder or not.
%% ---------------------------------------------------------------------------------
handle_call(check_seeder,{Pid,_},State) ->
	
    Reply = case ets:lookup(peer_state,Pid) of
			   [#state{pieces = Pieces}] ->
				   check_seeder(Pieces);
		       [] -> no_pid
		     end,
    {reply,Reply,State}.
			   
			 

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
	
%% ----------------------------------------------------------------------
%% when a process terminate , the table delete the peer with his peer state.
%% As the key is the pid of process.
%% ----------------------------------------------------------------------
handle_info({'DOWN',Ref,process,_,_},#monitors{peer_monitors = Monitor}=State) ->
	
	case dict:find(Ref,Monitor) of %% Check wheher it is in out monitor dictionary.
		{ok,Pid} ->
			ets:delete(peer_state, Pid),
			
		{noreply,State#monitors{peer_monitors = dict:erase(Ref,Monitor)}};
        error ->
             {noreply, State#monitors{peer_monitors = dict:erase(Ref,Monitor)}}
	end;

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ets:delete(peer_state).

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

