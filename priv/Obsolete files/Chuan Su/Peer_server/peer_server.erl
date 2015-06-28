%%% -------------------------------------------------------------------
%%% Author  : chuan su
%%% Description :
%%%
%%% Created : 2011-10-26
%%% -------------------------------------------------------------------
-module(peer_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%-include("states.hrl").
%% --------------------------------------------------------------------
%% External exports
%%-export([start/4]).
-export([start_link/1]).
-export([accept/1,accept_loop/6]).
-record(server_states, {port,                     %% listen port
					    ip = any,                 %% ip
					    listen_socket = null ,    %% listenning socket
					    connection_count =0,      %% current connections
                        max_connection =20,          %% max connection 
						fsm_module = fsm_socket_handler,          %% fsm handling module	,
					    my_peer_id	
						}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TCP_OPTIONS,[binary,{packet,2},
					 {active,false}]).


%% ====================================================================
%% External functions
%% ====================================================================
 
%% ====================================================================
%% Server functions
%% ====================================================================

start_link(My_peer_id)->
	Server_state=#server_states{port=1234,my_peer_id=My_peer_id},
	gen_server:start_link({local,peer_server}, ?MODULE, Server_state, []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(State = #server_states{port = Port,my_peer_id=My_peer_id}) ->
       
	process_flag(trap_exit,true),
	
	Ip_address= ip:get_ip_address(),
	
     case  gen_tcp:listen(Port,[binary,inet,{ip,Ip_address}]) of
     {ok,Listen_Socket} ->
		 
		 ok = inet:setopts(Listen_Socket, ?TCP_OPTIONS),

		{ok,Port_number}= inet:port(Listen_Socket),
		io:format("~w~n", [Port_number]),
		 
	{ok,{Address,_Port}}=inet:sockname(Listen_Socket),
		io:format("~w~n", [Address]),
       {ok,accept(State#server_states{listen_socket= Listen_Socket,port= Port_number,my_peer_id=My_peer_id})};
		 
     {error,Reason} ->
      
       {stop,{error_create_listen_socket,Reason}}

      end.

%% accept, spawn a new process
accept(State= #server_states{listen_socket = Listen_Socket,connection_count= Count,max_connection= Max,fsm_module = Module,my_peer_id=My_peer_id})->
	
	spawn(?MODULE,accept_loop,[self(),Listen_Socket,Count,Max,Module,My_peer_id]),
	
	io:format("~s~n", ["ok,spwan a new process for accepting connection"]),
	State.

accept_loop(Server,Listen_Socket,Count,Max,Module,My_peer_id)->
	
	io:format("~s~w~n", ["Current connection is ",Count]),
	{ok,Socket} = gen_tcp:accept(Listen_Socket),
	io:format("~s~n", ["accepe the client connection"]),
	io:format("~s~w~n",["Max connection is",Max]),
	   if 
		   Count+1>Max ->
			   io:format("reach the max connection"),
			   gen_tcp:close(Socket);
		   true ->
			   gen_server:cast(Server, {accept_newconnection,self()}),%% tell listener to accept another connection
			   
			  {ok,Pid}= peer_sup:start_fsm(My_peer_id),                         %% called from peer_sup module
			                                                          %% start a fsm process
			   
			gen_tcp:controlling_process(Socket, Pid),                 %% make the fsm process owner of the Socket.
			   
			                                                          %% and any message receive from the socket will be handler by the fs, process
	        
	         Module:set_client_socket(Pid,Socket)                     %% nitify the fsm to begin to receive packet
	   end.
	
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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% the server receives the notify that a connection has built and spawn another
%% process to accept the new connection.

handle_cast({accept_newconnection,_Pid}, State=#server_states{connection_count= Count}) ->
	
    {noreply, accept(State#server_states{connection_count= Count+1})};

%% the server receives the notify that a connection has closed
%% and decrease the current connection count

handle_cast({connection_closed,_Pid},State=#server_states{connection_count = Count}) ->
	
	{noreply,State#server_states{connection_count = Count-1}}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#server_states.listen_socket),
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

