%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description : This module is the main module for handshake,  all of the messages
%%%               received from peers ,downloading process as well as check sum function.
%%%
%%% Created : 2011-11-8
%%% -------------------------------------------------------------------
-module(built_connections_fsm).

-behaviour(gen_fsm).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,start_handshake/4,send_handshake/3,file_types/1]).

%% States exports
-export([send_handshake_state/2,handshake_state/2,message_state/2,check_sum_state/2]).

%% gen_fsm callbacks
-export([init/1,state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ----------------------------------------------------------------------
%% Records
%% ----------------------------------------------------------------------

-record(state,{socket,
			   info_hash,
			   download_index = null, %% requested piece index
			   check_sum_wrong = 0,   
			   block_amount,        
			   block_count = 0,    %% downloaded block amount
			   block = <<>>		%%  Current block we download.. 
			   }).

-define(PSTR,"BitTorrent protocol").
-define(TIMEOUT,100000).
-define(TIME_OUT,0).
-define(KEEP_ALIVE,timer:seconds(120)).
-define(Fault_COUNT,2).

%% ====================================================================
%% External functions
%% ====================================================================

file_types(List) ->
	case List of
		[{_,_}|_Tail] -> multiple_file;
		_ -> single_file
	end.

write_to_file(Index,Piece)->
		File = torrent_info:fetch_path(),
			case file_types(File) of
				single_file ->
					
		       spawn(fun() -> 
					   FileName = torrent_info:fetch_name(),
					   file_handler:open(FileName,Index,Piece) end);
				multiple_file ->
			         spawn(fun() -> multiple_file_handler:piece_store(Index, Piece) end)
			end.
			

%% Send the handshake to peers.
send_handshake(Socket,Info_hash,My_peer_id)->
	
	 Header= get_header(),
try
	 gen_tcp:send(Socket,Header),
	 gen_tcp:send(Socket,Info_hash),
	 gen_tcp:send(Socket,My_peer_id),
	 
     {ok,handshake_complete}

catch
   error:_->
    {error,instance}

end.

%% Get the header of BitTorrent Handshake message.
get_header()->
	
  Pstrlen= length(?PSTR),
  Reserved_byte = <<0:64/big>>,
  <<Pstrlen:8,?PSTR,Reserved_byte/binary>>.

%% Get random piece index to request.

random_index(List)->
	Length = length(List),
	if Length >0 ->
	Random_index = random:uniform(length(List)),
	Piece = lists:nth(Random_index, List),
    case torrent_request_server:is_requested(Piece) of %% Check if this index has already been requested.
		true -> random_index(List--[Piece]);
		false -> Piece
	end;
	   true ->
		   no_needed_pieces
	end.
%% -----------------------------------------------------------------
%% send intersted message to this peer  or not interested message.
%% -----------------------------------------------------------------
send_interested(#state{socket = Socket} = StateData) ->
	case local_interested() of 
		true ->
		    gen_tcp:send(Socket, torrent_message:outgoing(interested)),
			peer_state:am_interested(),
		%%	io:format("~s~n", ["ok,I am interested this peer"]),
			{next_state,message_state,StateData};
		false ->
			gen_tcp:send(Socket,torrent_message:outgoing(not_interested)),
			{stop,normal,StateData}
	end.
			
%% ----------------------------------------------------------------		
%% Check whether we interested this peer.
%% ----------------------------------------------------------------
local_interested() ->
	
	Local = torrent_piece_table:get_need_pieces(), %% get our local pieces.(the pieces we already have).
	{ok,Remote} = peer_state:get_remote_pieces(), %% get the remote pieces (the pieces this peer have).
	Pred = fun(X) ->
				   lists:member(X, Remote)
		   end,
	lists:any(Pred, Local).


%% ====================================================================
%% Handle message  functions
%% ====================================================================


%% --------------------------------------------------------------------------
%% The bitfield message will be the first message we received after handshake.
%% ---------------------------------------------------------------------------
handle_message({bitfield,BitField},StateData) ->
	

	%% first check the bitfield is a valid bitfield.
	case torrent_piece_handler:check_bitfield({bitfield,BitField}) of 
		true ->
			%% update the peer state.
			peer_state:maintain_peer_pieces(BitField),
	        send_interested(StateData);
		
        false ->
            %% the bitfield is not valid, drop the connection.
             {stop,wrong_bitfield,StateData}
        end;

%% ----------------------------------------------------------------
%% Deal with the "have" message
%% Some clients using "lazy-bitfield",
%% We will receive many "have,message".
%% ----------------------------------------------------------------
handle_message({have,Index},StateData) ->
	
	%% update the peer state.
	peer_state:update_peer_pieces(Index),
	
	%%io:format("~p~n",[{have,Index}]),
	{next_state,message_state,StateData};

%% ------------------------------------------------------------------
%% handle keep_alive message
%% ------------------------------------------------------------------
handle_message(keep_alive,StateData) ->
	
	io:format("~s~n", ["keep alive time"]),
	%% after 2 minuts, we will drop the connection ..if no message received.
	{next_state,message_state,StateData,?KEEP_ALIVE};


%% -------------------------------------------------------------------
%% handle "unchoke" message
%% Begin download if receive "unchoke" message. 
%% --------------------------------------------------------------------
handle_message(unchoke,#state{socket = Socket,
							  download_index = P_index} = StateData) ->
	
	  peer_state:peer_chock_state(unchoke), %% Update the peer state.
	  io:format("~s~n",["unchoke"]),
	  
	case P_index of 
		
		Piece_index when is_atom(Piece_index) -> %% when Piece_index == null, means this is the first
			                                     %% time we download from this peer, and start requesting.
			
		%% Initial the request message	
	        Pieces = peer_state:get_download_pieces(),
	  
	        case random_index(Pieces) of
		 
		        Index when is_integer(Index)->
					 
					 %% Get a list of chunks of the piece from chunk_handler.
					
	                  Chunks = torrent_chunk_handler:get_chunks(Index),
					  Chunks_amount = torrent_chunk_handler:get_chunk_amount(Index),
					  
	               %% before downloading , tell the request server, we are going to download this piece.
					  
                      torrent_request_server:insert_request(Index),
			   %% spawn download process ,send a set of request chunks to this peer .
			         spawn(fun()->
	                 [gen_tcp:send(Socket, torrent_message:outgoing(Chunk))
	                 || Chunk <- Chunks]
		          end),
		      %% Update the parameters of this process.
	            {next_state,message_state,StateData#state{download_index = Index,
														  block_amount = Chunks_amount,
														  block = <<>> 
														}};
		        _ ->
			     {stop,normal,StateData}
	         end;
		%% If the piece is downloading and receive a "unchoke" message ,just keep receiving blocks..
	Piece_index when is_integer(Piece_index) ->
		
			 {next_state,message_state,StateData}
	end;

%% ---------------------------------------------------------------------------------
%% handle "choke" message
%% When a "choke" message received , wait the unchoke message
%% ----------------------------------------------------------------------------------
handle_message(choke,StateData) ->
	
	%% update the peer state.
	peer_state:peer_chock_state(choke),
	io:format("~s~n",["choke"]),
	{next_state,message_state,StateData};

%% -------------------------------------------------------------------------------
%% handle "piece " message invoked, When a block or a chunk downloaded.
%% --------------------------------------------------------------------------------
handle_message({piece,Index,Begin,Block},#state{socket = Socket,
												download_index = Request_index,
												block_amount = Max,
												block_count = Count,
                                            block = Current} =StateData) ->	
	
	%% First we should make sure the receiving piece index is the same index we requested.
   case Index == Request_index  of 
	
     true ->
		 
		if Count+1 < Max -> %% this means we have not received the whole piece,
			                %% keep receiving the blocks 
			   
			io:format("~p", [{Index,Begin}]),
            {next_state,message_state,StateData#state{block_count = Count+1,
													 block =  <<Current/binary,Block/binary>>}};
		 true ->
			 %% We received the whole piece and send keep_alive message to 
			 %% this peer and go to the check sum state to check the piece 
			 
			gen_tcp:send(Socket, torrent_message:outgoing(keep_alive)),
		   {next_state,check_sum_state,StateData#state{socket = Socket,
													   block =  <<Current/binary,Block/binary>>},?TIME_OUT}
          end;
	  
     false ->

	 {stop,normal,StateData}
    end.


%% ====================================================================
%% Server functions
%% ====================================================================

start_link()->
	gen_fsm:start_link(?MODULE, [], []).


%% Triggar the init state and start the fsm states for handshake.

start_handshake(Pid,Socket,Info_hash,My_peer_id)->
	
	gen_fsm:send_event(Pid, {start_handshake,Socket,Info_hash,My_peer_id}).


%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([]) ->
	
	process_flag(trap_exit,true),
    {ok, send_handshake_state, #state{download_index = null,
									  check_sum_wrong =0,
									  block_count = 0,
									  block = <<>>}}.

%% --------------------------------------------------------------------
%% Func: send_handshake_state/2
%% Returns: {next_state,   handshake_state, NextStateData, Timeout} |
%%          {stop, handshake_error, NewStateData}
%% This state is for sending handshake to peers
%% --------------------------------------------------------------------

send_handshake_state({start_handshake,Socket,Info_hash,My_peer_id},StateData) ->
	
	inet:setopts(Socket, [{active,false},{packet,0},binary]),

	
	case send_handshake(Socket,Info_hash,My_peer_id) of 
		
		{ok,handshake_complete}-> 
			
			%%io:format("ok,send handshake,completed"),
			
	       {next_state,handshake_state,StateData#state{
												 socket = Socket,info_hash = Info_hash},?TIME_OUT};
		
		{error,instance}->
         %%  io:format("no,handshake error"),
		   
	       {stop,handshake_error,StateData}
	end.


%% --------------------------------------------------------------------
%% Func: handshake_state/2
%% Returns: {next_state,   message_state, NextStateData, Timeout} |
%%          {stop, handshake_error, NewStateData}
%% Receive the handshake back and check whether it is corrent for us.
%% --------------------------------------------------------------------
handshake_state(timeout,#state{socket = Socket,info_hash = Info_hash}=StateData) ->
	
	case gen_tcp:recv(Socket, 68) of
		
		{ok,<<19, ?PSTR, _ReservedBytes:8/binary, Info:20/binary, _Id:20/binary>> }
		  when Info == Info_hash  ->
			
		%% set socket options to {active,once}.
		inet:setopts(Socket, [{packet, 4}, {active, once}, {packet_size, 33000}]), 
		
		%% The handshake success, go to the next state, and receive bitfield message and so on..
	       {next_state,message_state,StateData};
		
		{ok,Binary} when is_binary(Binary) ->
			
			{stop,normal,StateData}%% drop the connections as the handshake is wrong .
	end.

%% --------------------------------------------------------------------
%% Func: message_state/2
%% Returns: {next_state,   check_sum_state, NextStateData, Timeout} |
%%          {stop, handshake_error, NewStateData}
%% --------------------------------------------------------------------

message_state({packet,Packet},StateData)-> 
	
	case torrent_message:incoming(Packet) of  
		
		{bitfield,BitField} ->
			handle_message({bitfield,BitField},StateData); %% handle "bitfield" message.
		
		{have,Piece_index} ->
			handle_message({have,Piece_index},StateData); %% handle "have" message.
			
		keep_alive ->
			handle_message(keep_alive,StateData);  %% handle "keep_alive" message.
		
		unchoke ->
			handle_message(unchoke,StateData);     %% handle "unchoke" message.
			
		choke -> 
			handle_message(choke,StateData);   %% handle "choke" message
		
		{piece,Index,Begin,Block} ->
			handle_message({piece,Index,Begin,Block},StateData) %% hand "piece" message.
			
	end;
%% -----------------------------------------------------------------------------------------------
%% when receive "keep_alive" message, drop the connections if no message received within 2 minutes.
%% ------------------------------------------------------------------------------------------------
message_state(timeout,StateData) ->
	
	{stop,keep_alive_timeout,StateData}.

%% --------------------------------------------------------------------
%% Func: check_sum_state/2
%% Returns: {next_state,   message_state, NextStateData, Timeout} |
%%          {stop, normal, NewStateData}
%% After receiving the whole piece, check sum state will be invoked
%% --------------------------------------------------------------------

check_sum_state(timeout,#state{socket = Socket,
							   download_index = Index,
							   check_sum_wrong = Fault_count,
							   block = Piece} = StateData) ->
	
	Hash = torrent_piece_handler:get_piece_hash(Index), %% get the hash of specified piece
   
	io:format("~s~n", ["Check_sum, state invoked"]),
	
	case crypto:sha(Piece) == Hash of  %% check if the downloaded hash match the hash from torrent file
		
		true ->
			
			torrent_piece_table:update_piece(Index), %% Update the local table that we have this piece/
			torrent_request_server:cancle_request(Index), %% Update the request server.
		   torrent_download_records:download(byte_size(Piece)), %% Update the download records and record
			                                                    %% the bytes we downlodaed.
			
			%% file handler......spwan a process for each piece...
			write_to_file(Index,Piece),
		
			io:format("~s~n", ["Hahahahaah,successful!!!!"]),
			
	        Pieces = peer_state:get_download_pieces(), %% Ask for the pieces list that is available for us 
			                                           %% to download from this peer.
			
			case random_index(Pieces) of
				
				 Piece_Index when is_integer(Piece_Index) ->
					 
					 %% get a set of chunks of specofied piece index , from torrent_chunk_handler.
	                 Chunks = torrent_chunk_handler:get_chunks(Piece_Index),
				     Chunks_amount = torrent_chunk_handler:get_chunk_amount(Piece_Index),
				     
					 %% before downloading , tell the request server, we are going to download this piece.
					 
                     torrent_request_server:insert_request(Piece_Index),
				     
					 %% spwan a download process.
				     spawn(fun()->
	                 [gen_tcp:send(Socket, torrent_message:outgoing(Chunk))
	                 || Chunk <- Chunks]
	 	             end),
		         
				 %%  A new download start , initial the parameters again...
				 %%  and go to the message state. to handle message sent from this peer.
	            {next_state,message_state,StateData#state{ check_sum_wrong = 0,
														  download_index = Piece_Index,
														   block_amount =  Chunks_amount,
											               block_count = 0,
											               block = <<>> }};	
				_ -> 
					{stop,normal,StateData} %% drop the connections as there is no more available
			                                %% piece for us to download from this peer.
			     end;
		 %% The hash of  download piece  is mot match 
		   false ->
			   
			    case Fault_count =< ?Fault_COUNT of 
					true ->
						
		    	io:format("~s~n", ["NO, HASH IS WORONG"]),
				
              %% Download from this piece again from this peer
	           Chunks = torrent_chunk_handler:get_chunks(Index),
	
	           spawn(fun()->
	           [gen_tcp:send(Socket, torrent_message:outgoing(Chunk))
	           || Chunk <- Chunks]
		        end),
		    {next_state,message_state,StateData#state{download_index = Index,
													check_sum_wrong = Fault_count +1,
											        block_count = 0,
  			           					           block = <<>> }};
					
				%% if the hash is wrong again , drop the connections 
					false ->		
						{stop,normal,StateData}
				end
	end.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name(_Event, _From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% As this process is the owner of the socket ,so we will receive 
%% message as {tcp,Socket,Packer}.
%% --------------------------------------------------------------------
handle_info({tcp,Socket,Packet}, StateName, #state{socket= Socket}=StateData) ->
	
	
	
	inet:setopts(Socket, [{active,once}]), %% Flow controll : enable to receive next TCP message
	
	?MODULE:StateName({packet,Packet},StateData); %% Trigger the current state to handle the message


handle_info({tcp_closed,Socket},_StateName,#state{socket = Socket}= StateData)->
	
	{stop,normal,StateData}; %% close the connection


handle_info(_Info,StateName,StateData)->
	
	{noreply, StateName,StateData}.


%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% Close the socket.
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket}) ->
	
	(catch gen_tcp:close(Socket)),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

