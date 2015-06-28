%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description : Request server, maintains each requst piece index
%%%               for avoiding duplicated request.
%%% Created : 2011-12-7
%%% -------------------------------------------------------------------
-module(torrent_request_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,is_requested/1,insert_request/1,cancle_request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {request_list = []}).

-define(FLUSH_TIME,timer:seconds(60)).

%% ====================================================================
%% External functions
%% ====================================================================

new()->
	[].
%% ====================================================================
%% Server functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

is_requested(Piece)->
	gen_server:call(?MODULE, {check_request,Piece}).

insert_request(Piece)->
	gen_server:cast(?MODULE,{request,Piece}).

cancle_request(Piece)->
	gen_server:cast(?MODULE, {cancle_request,Piece}).


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	%%erlang:start_timer(?FLUSH_TIME, ?MODULE, flush_request_list),
	erlang:send_after(?FLUSH_TIME, self(), flush_request_list),%% start a timer for flush the request in 1minute
	
    {ok, #state{request_list = []}}.


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
handle_call({check_request,Piece}, From, #state{request_list = Request} = State) ->
	
    Reply =	lists:member(Piece, Request) ,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({request,Piece}, #state{request_list = Request} =State) ->
	
    {noreply, State#state{request_list = Request++[Piece]}};



handle_cast({cancle_request,Piece},#state{request_list = Request} =State)->
	
    {noreply, State#state{request_list = Request--[Piece]}}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(flush_request_list,State) ->

     erlang:send_after(?FLUSH_TIME, self(), flush_request_list),
    {noreply, State#state{request_list = new()}}.


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

