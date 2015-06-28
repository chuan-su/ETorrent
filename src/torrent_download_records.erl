%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description :
%%%              A recorder for recording the bytes downloaded and bytes left and uploaded
%%% Created : 2011-12-8
%%% -------------------------------------------------------------------
-module(torrent_download_records).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,get_download_amount/0,get_uploaded_amount/0,
		  get_left_amount/0,download/1,upload/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,code_change/3]).

-record(state, {upload = 0,
                download = 0,
				file_length,
				bytes_left}).

%% ====================================================================
%% External functions
%% ====================================================================
calculate_amount_left(File_length,Download) ->
	Left = File_length - Download,
	Left.

%% ====================================================================
%% Server functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

get_download_amount() ->
	gen_server:call(?MODULE, get_downloaded_amount).

get_uploaded_amount() ->
	gen_server:call(?MODULE, get_uploaded_amount).

get_left_amount() ->
	gen_server:call(?MODULE, get_left_amount).

download(Bytes_size) ->
	gen_server:cast(?MODULE, {downloaded,Bytes_size}).

upload(Bytes_size) ->
	gen_server:cast(?MODULE, {uploaded,Bytes_size}).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	File_length = torrent_info:fetch_length(),
    {ok, #state{upload = 0,
				download = 0,
				file_length = File_length,
				bytes_left = File_length}}.

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
handle_call(get_downloaded_amount, From, #state{download = Download,
												file_length = Length} = State) ->
	Reply = case Download == Length of
		        true ->
                  completed;
		        false ->
			      Download
			end,
    {reply, Reply, State};

handle_call(get_uploaded_amount,From,#state{upload = Upload} = State) ->
	Reply = Upload,
	{reply,Reply,State};

handle_call(get_left_amount,From,#state{bytes_left = Left} = State) ->
	Reply = Left,
	{reply,Reply,State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({downloaded,Bytes}, #state{download = Download,
									   file_length = Length} = State) ->
	New_download = Download+Bytes,
	Left = calculate_amount_left(Length,New_download),
    {noreply, State#state{download = New_download,bytes_left = Left}};


handle_cast({uploaded,Bytes},#state{upload = Upload} = State) ->
	
	{noreply,State#state{upload = Upload+Bytes}}.

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
