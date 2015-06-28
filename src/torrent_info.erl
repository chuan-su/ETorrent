%%% -------------------------------------------------------------------

%%% Author  : Chuan Su
%%% Description :
%%%
%%% Created : 2011-11-21
%%% -------------------------------------------------------------------
-module(torrent_info).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([fetch_info_hash/0,fetch_announce/0,fetch_announce_list/0,fetch_piece_length/0,
		 fetch_pieces/0,   fetch_length/0,  fetch_name/0,         fetch_path/0]).

%% state records include two dictionary
-record(state, {file_dict,              %% torrent file dictionary
				info_dict}).            %% info dictionary

%% ------------------------------------------------------------------
%% Gen_server init/1 ,call back functions
%% Get the two dictionary from torrent file
%% ------------------------------------------------------------------

parse_file(File)->
	Bin=filverk:read(File),
	filverk:decode(Bin).


get_info(Dict)->
	dict:fetch(<<"info">>, Dict).

%% ====================================================================
%% Handle_call ,call back functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Get info hash of torrent file ,which is not url encoded
%% --------------------------------------------------------------------
get_info_hash(Dict)->
	
	List=filverk:encode(Dict),
	filverk:hash(List).

%% Get announce url
get_announce(Dict)->
	binary_to_list(dict:fetch(<<"announce">>,Dict)).

%% -------------------------------------------------------------
%% Get announce-list from torrent file if it exits
%% Handle function for retrieving announce list
%% -------------------------------------------------------------

get_announce_list(Dict)->
	
	try dict:fetch(<<"announce-list">>,Dict) of
		List ->
			handle_list(List,[])
	catch                              %% Returns annnounce url if announce-list doesn't exit.
		error:_ ->
			get_announce(Dict)
	end.

handle_list([Head|Tail],Accu)->
    case Head of
         [Binary]->
         handle_list(Tail,[[binary_to_list(Binary)]|Accu])
         end;

handle_list([],Accu)->
	
  lists:reverse(Accu).

%% Get piece length
get_piece_length(Dict)->
	dict:fetch(<<"piece length">>,Dict).

%% Get pieces from torrent file
get_pieces(Dict)->
	dict:fetch(<<"pieces">>,Dict).


%% get file name
get_name(Dict)->
	Binary = dict:fetch(<<"name">>, Dict),
	binary_to_list(Binary).

%% -----------------------------------------------------------------------
%% Get file paths
%% Deal with two types of torrent file , Single File and Multiple File
%% -----------------------------------------------------------------------

get_file_path(Dict)->
	
	try dict:fetch(<<"files">>, Dict) of
		File_list ->
			case files_path(File_list) of
				[One] ->
					One;
				List when is_list(List) ->
					Filename= get_name(Dict),
			[{filename:join([Filename,Path]),Size} || {Path,Size} <- List]
		end
	catch
		error:badarg ->       %% Return file name if it is Single File
			get_name(Dict)
	end.
					
files_path(File_list) when is_list(File_list) ->
	
[file_path(Dict) || Dict <- File_list].

file_path(Dict)->
	
Path_list = dict:fetch(<<"path">>,Dict),
Size = dict:fetch(<<"length">>,Dict),
Path = filename:join([binary_to_list(P)|| P <- Path_list]),
{Path,Size}.

%% ---------------------------------------------------------------
%% Get torrent file length
%% Deal with two types of torrent file, Single File,Multiple File
%% ---------------------------------------------------------------

get_length(Dict)->    
	
	try  dict:fetch(<<"files">>, Dict) of
		File_List ->
			file_length(File_List)
		catch 
			error:badarg ->
				dict:fetch(<<"length">>,Dict)
			end.

file_length(File_list) when is_list(File_list)->
	
    Sum	= lists:sum([get_file_length(File)|| File <- File_list]),
    Sum.

get_file_length(Dict)->
	
	case dict:fetch(<<"length">>, Dict) of
	Length when is_integer(Length)->
		Length;
	Length when is_list(Length) ->
		list_to_integer(Length)
	end.

%% ====================================================================
%% Server functions,
%% Fetch each value of torrent file
%% ====================================================================

start_link(File)->
	gen_server:start_link({local,?MODULE}, ?MODULE, File, []).

fetch_info_hash()->
	Reply = gen_server:call(?MODULE, info_hash),
    Reply.

fetch_announce()->
	Reply = gen_server:call(?MODULE, announce),
    Reply.

fetch_announce_list()->
	Reply = gen_server:call(?MODULE, announce_list),
    Reply.

fetch_piece_length()->
	Reply = gen_server:call(?MODULE, piece_length),
    Reply.

fetch_pieces()->
	Reply = gen_server:call(?MODULE, pieces),
    Reply.

fetch_length()->
	Reply = gen_server:call(?MODULE, length),
    Reply.

fetch_name()->
	Reply = gen_server:call(?MODULE, name),
    Reply.

fetch_path()->
	Reply = gen_server:call(?MODULE, path),
    Reply.
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(File) ->
	
	process_flag(trap_exit,true),
	File_dict=parse_file(File),
	Info_dict=get_info(File_dict),
	State=#state{file_dict=File_dict,info_dict=Info_dict}, %% Maitains two dictionary
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

handle_call(info_hash,From,#state{info_dict=Info_dict}=State)->
	
	Info_hash=get_info_hash(Info_dict),
	{reply,Info_hash,State};

handle_call(announce,From,#state{file_dict=File_dict}=State)->
	Announce = get_announce(File_dict),
	{reply,Announce,State};

handle_call(announce_list,From,#state{file_dict=File_dict}=State)->
	Announce_list=get_announce_list(File_dict),
	{reply,Announce_list,State};

handle_call(piece_length,From,#state{info_dict=Info_dict}=State)->
	
    Piece_length= get_piece_length(Info_dict),
	{reply,Piece_length,State};

handle_call(pieces,From,#state{info_dict=Info_dict}=State)->
	Pieces = get_pieces(Info_dict),
	{reply,Pieces,State};

handle_call(length,From,#state{info_dict=Info_dict}=State)->
	Length = get_length(Info_dict),
	{reply,Length,State};

handle_call(name,From,#state{info_dict=Info_dict}=State)->
	FileName = get_name(Info_dict),
	{reply,FileName,State};

handle_call(path,From,#state{info_dict=Info_dict}=State)->
	File_path = get_file_path(Info_dict),
	{reply,File_path,State};

handle_call(_Request,From,State)->
	{reply,error,State}.

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

