%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description : Chunk handler, divide each piece to chunks 
%%%
%%% Created : 2011-12-7
%%% -------------------------------------------------------------------
-module(torrent_chunk_handler).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,get_chunks/1,get_chunk_amount/1,get_chunk_size/1,last_piece_chunks/5]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last_piece_index ,
				chunks = [],
				last_piece_chunks =[]
				}).

-define(CHUNK_LENGTH,32*1024).
-define(OFF_SET,0).

%% ====================================================================
%% External functions
%% ====================================================================

%% divide the normal piece to chunks. [{Offset,Length}|.........]
chunks(Offset,Chunk_length,Piece_length,Acc)->
	
	case Offset+Chunk_length == Piece_length of 
		true  ->
			lists:reverse([{Offset,Chunk_length}|Acc]);
		false -> 
			chunks(Offset+Chunk_length,Chunk_length,Piece_length,[{Offset,Chunk_length}|Acc])
	end.

%% divide the last piece to chunks. -> [{Offset,Length}|.........]
last_piece_chunks(Offset,Chunk_length,Last_Chunk_length,Last_piece_length,Acc) ->
	
	case Offset+Chunk_length >= Last_piece_length of 
		 true -> 
			 lists:reverse([{Offset,Last_Chunk_length}|Acc]);
		 false ->
 last_piece_chunks(Offset+Chunk_length,Chunk_length,Last_Chunk_length,Last_piece_length,[{Offset,Chunk_length}|Acc])
	 end.

%% ====================================================================
%% Server functions
%% ====================================================================
start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

get_chunks(Index)->
	gen_server:call(?MODULE, {get_chunks,Index}).

get_chunk_amount(Index) ->
	gen_server:call(?MODULE, {get_chunk_amount,Index}).

get_chunk_size(Index) ->
	gen_server:call(?MODULE, {get_chunk_size,Index}).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	
	File_length = torrent_info:fetch_length(), %% get file length
	Piece_amount = torrent_piece_handler:piece_amount(), %% get piece amount in the file.
	
	Piece_Length = torrent_info:fetch_piece_length(), %% get piece length
	Chunks = chunks(0,?CHUNK_LENGTH,Piece_Length,[]), %% return a list of chunks.
	
	Last_piece_length = File_length -(Piece_Length*(Piece_amount-1)), %% get last piece length
	Last_chunk_length = Last_piece_length rem (?CHUNK_LENGTH), %% get last chunk length in last piece
	
   %% return  a list of chunks in last piece
	Last_piece_chunks = last_piece_chunks(?OFF_SET,?CHUNK_LENGTH,Last_chunk_length,Last_piece_length,[]),
	
	
	
    {ok, #state{last_piece_index = Piece_amount-1,
				chunks = Chunks,   %% a list of Chunks
				last_piece_chunks = Last_piece_chunks}}. %% a list of chunks in last piece.

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

handle_call({get_chunks,Index}, From, #state{last_piece_index = Last,
											 chunks = Chunks,
											 last_piece_chunks = Last_piece_chunks}=State) ->
	
	%% check whether the request index is the last piece or not.
	Reply = case Index == Last of 
				
		           true ->
			             [{request,Last,Offset,Length}||{Offset,Length} <- Last_piece_chunks];
		           false ->
			             [{request,Index,Offset,Length}||{Offset,Length} <- Chunks]
	         end,
    %% Reply is a list [{request,Index,Offset,Length}|........]
    {reply, Reply, State};


%% return the number of  chunks in specified piece index. 
handle_call({get_chunk_amount,Index},From,#state{last_piece_index = Last,
												 chunks = Chunks,
												 last_piece_chunks = Last_piece_chunks} = State) ->
	Reply = case Index == Last of
				true ->
					  length(Last_piece_chunks);
				false ->
					  length(Chunks)
			end,
	{reply,Reply,State};


%% get the chunk size.
handle_call({get_chunk_size,Index},From,#state{last_piece_index = Last,
											   last_piece_chunks = Last_piece_chunks} = State) ->
	
	 Reply = case Index == Last of 
			     true ->
			        {_Offset,Last_Chunk_length} = lists:last(Last_piece_chunks),
			         [{chunk_length,?CHUNK_LENGTH},{last_chunk_lengh,Last_Chunk_length}];
		         false ->
			        {chunk_length,?CHUNK_LENGTH}
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
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

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

