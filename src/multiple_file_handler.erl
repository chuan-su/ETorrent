%%% -------------------------------------------------------------------
%%% Author  : Chuan Su, Yuwei chen
%%% Description :
%%%
%%% Created : 2011-12-15
%%% -------------------------------------------------------------------
-module(multiple_file_handler).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,piece_store/2,get_piece_position/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-record(state, {piece_position = []}).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%%  This function is to write each piece(data) to the conrresponding
%%  file.
%% --------------------------------------------------------------------

piece_store(Index,Block)->
	List = ets:lookup(positions, Index),
	binary_store(List,Block).

%% some piece will cross two or more files..
binary_store([{Path,_,Position,Length}|Tail],Binary)->
	case Binary of
		<<Data:Length/binary,Rest/binary>> ->
	         storage_dir:files(Path, Position, Data),
	         binary_store(Tail,Rest)
	 end;
binary_store([],<<>>) ->
	ok.

%% ----------------------------------------------------------------------
%% Before starting downloading, we first make a piece position map
%% and specify the start position in the file and the length 
%% for each piece.
%% -----------------------------------------------------------------------
piece_position(Piece_Index,Piece_begin,Piece_length,File_begin,[{Path,Length}|Tail]= List) ->
	
	  Piece_End = Piece_length - Piece_begin,
	  if (File_begin+Piece_End)< Length ->
			 Position = {Path,Piece_Index,File_begin,Piece_End},
			 NewFile_begin = File_begin+Piece_End,
			 [Position|piece_position(Piece_Index+1,0,Piece_length,NewFile_begin,List)];
		 (File_begin+Piece_End)> Length ->
			 File_end = Length - File_begin,
			 Position = {Path,Piece_Index,File_begin,File_end},
			 NewPiece_begin = Piece_begin+File_end,
			 [Position|piece_position(Piece_Index,NewPiece_begin,Piece_length,0,Tail)];
		 true ->
			 Position = {Path,Piece_Index,File_begin,Piece_End},
			 [Position|piece_position(Piece_Index+1,Piece_begin,Piece_length,0,Tail)]
	  end;

piece_position(_,_,_,_,[])->
	[].

%% ====================================================================
%% Server functions
%% ====================================================================
start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% get piece position in corresponding file and the length occupid in that file

get_piece_position(Index)->
	gen_server:call(?MODULE,{get_piece_position,Index}).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	
	Piece_length = torrent_info:fetch_piece_length(), %% fetch piece length
	File_list = torrent_info:fetch_path(), %% this will return [{File_Path,Length}|.........]
	
	Piece_position = piece_position(0,0,Piece_length,0,File_list),
	
	%% Here  creat an ets table which maintains the whole piece positions 
	ets:new(positions, [named_table,bag,{keypos,2}]),
	
	%% Initial the ets table ,traverse each each positions and insert them to ets table,
	Add = fun({Path,Index,Position,Length}) ->
				  ets:insert(positions, {Path,Index,Position,Length})
		  end,
	lists:foreach(Add, Piece_position),
	
	%% Create the main folder 
	Main_foleder = torrent_info:fetch_name(),
	storage_dir:main_folder(Main_foleder),
	
	%% Init ends.
    {ok, #state{piece_position = Piece_position}}.

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

handle_call({get_piece_position,Index},From,#state{piece_position = Piece_position} = State) ->
	
	 Reply = ets:lookup(positions, Index),
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
    ets:delete(positions).

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

