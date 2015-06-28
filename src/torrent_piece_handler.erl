%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :  piece handler , maintains the piece number and bitfield size
%%%                 as well as the hash of each piece.
%%%
%%% Created : 2011-12-5
%%% -------------------------------------------------------------------
-module(torrent_piece_handler).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,piece_amount/0,check_bitfield/1,get_piece_hash/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {piece_hashes,
				piece_amount,
				bitfield_size,
				spare_length}).

%% ====================================================================
%% External functions
%% ====================================================================

%% get the number of the piece in a torrent file
piece_amount(Hashes) ->
	byte_size(Hashes) div 20.


%% calculate the bitfield size.
bitfield_length(Piece_amount) ->
	
	Bitfield_length = Piece_amount div 8,
	case spare_bitfield_length(Piece_amount) of 
		0 -> Bitfield_length;
        _ -> Bitfield_length+1
end.

%% calculate the spare bitfield length
spare_bitfield_length(Piece_amount) ->
	Length = 8-(Piece_amount rem 8),
	case Length of
        0 -> 0;
		_ -> Length
	end.

%% Check whether the last spare bits in bitfield  are 0.
check_last_byte(Byte,Spare_length)->
	Byte_list = bitfield:convert(Byte),
	List      = lists:nthtail((8-Spare_length), Byte_list),
	Pred = fun(X) ->
				   if X == 0 -> true;
					  true   -> false
				   end
		   end,
   lists:all(Pred, List).


%% ====================================================================
%% Server functions
%% ====================================================================

start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE,[],[]).

piece_amount() ->
	gen_server:call(?MODULE,get_piece_amount).

check_bitfield({bitfield,BitField})->
	gen_server:call(?MODULE, {check_bitfield,BitField}).

get_piece_hash(Index) ->
	gen_server:call(?MODULE,{get_piece_hash,Index}).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	
	Hashes = torrent_info:fetch_pieces(),
	Piece_amount = piece_amount(Hashes),
	Bitfield_length = bitfield_length(Piece_amount),
	Spare_length = spare_bitfield_length(Piece_amount),
	
    {ok, #state{piece_hashes = Hashes,
				piece_amount = Piece_amount,
				bitfield_size = Bitfield_length,
				spare_length = Spare_length}}.


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

handle_call(get_piece_amount,From,#state{piece_amount = Amount}=State) ->
	
	{reply,Amount,State};

handle_call({check_bitfield,BitField}, From, #state{bitfield_size = Size,
													spare_length = Length} = State) ->
    Reply =	if 
		byte_size(BitField) == Size ->
			check_last_byte(binary:last(BitField),Length);
		true ->
			false
	end,
    {reply, Reply, State};

%% ----------------------------------------------------------------------
%% Return the hash for specified piece ,this is for checking sum when 
%% download a whole piece.
%% ----------------------------------------------------------------------

handle_call({get_piece_hash,Index},From,#state{piece_hashes = Hashes} 
		                                                      = State) ->
	Offset = 20*Index,
	
	Reply = case Hashes of 
		  <<_:Offset/binary,Hash:20/binary,_/binary>> -> Hash;
		  _ -> {error,instance}
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

