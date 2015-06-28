%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description : piece table maintains the local piece we have and
%%%               update the piece statue when a piece downloaded.
%%%               we can know what pieces we still need to download..
%%% Created : 2011-12-6
%%% -------------------------------------------------------------------
-module(torrent_piece_table).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,update_piece/1,get_local_pieces/0,get_need_pieces/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {table,
                piece_number,
				piece_index_list}).

-define(TABLE_NAME,piece_table).
%% ====================================================================
%% External functions
%% ====================================================================

%% Initial the table with key-value pair {Index,0}
%% 0 represents we do not have this piece.
initial_piece_table(Table,Pieces) ->
	
	[ets:insert(Table, {Index, 0}) || Index <- Pieces].

%% ====================================================================
%% Server functions
%% ====================================================================
start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

get_local_pieces() ->
	gen_server:call(?MODULE,local_pieces).

get_need_pieces() ->
	gen_server:call(?MODULE,need_pieces).

update_piece(Piece_Index) ->
	gen_server:cast(?MODULE,{update_piece,Piece_Index}).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	
	Table_name = ets:new(pieces, [set,private]), %% create a new table
	Total = torrent_piece_handler:piece_amount(),
	Pieces = lists:seq(0, Total - 1),
	
	initial_piece_table(Table_name,Pieces),
	
	State = #state{table = Table_name,
				piece_number =  Total,
				piece_index_list = Pieces},
	
    {ok, State}.

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


%% return  a list of pieces which the Status is 0 in {Index,Status} pair.
%% as Status 0 represents we do not have this piece, we need it.
handle_call(need_pieces,From,#state{table = Table,
									piece_index_list = Indexes} = State) ->
	
	Table_list = [{I,ets:lookup_element(Table,I,2)}|| I <- Indexes],
	Fun = fun({I,A},Acc) ->
				  if A == 0 -> [I|Acc];
					 true   -> Acc
				  end end,
	Local = lists:foldl(Fun,[], Table_list), %% returns an Acc 
    Reply = lists:sort(Local),
    {reply, Reply, State};


%% return  a list of pieces which the Status is 1 in {Index,Status} pair.
%% as Status 1 represent we have this piece
handle_call(local_pieces, From, #state{table = Table,
									   piece_index_list = Indexes} = State) ->
	
	Table_list = [{I,ets:lookup_element(Table,I,2)}|| I <- Indexes],
	Fun = fun({I,A},Acc) ->
				  if A == 1 -> [I|Acc];
					 true   -> Acc
				  end end,
	Local = lists:foldl(Fun,[], Table_list),
    Reply = lists:sort(Local),
    {reply, Reply, State}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% Update the piece status when download a whole piece.

handle_cast({update_piece,Index}, #state{table = Table} = State) ->
	 Status = ets:lookup_element(Table, Index, 2),
	 case Status == 0 of 
		 true -> 
			 ets:update_counter(Table, Index, 1),
	         {noreply, State};
		 false ->  
			 {noreply, State}
	 end.

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
    ets:delete(pieces).

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

