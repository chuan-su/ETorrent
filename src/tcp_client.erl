%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description : This module is for connect peers.
%%%
%%% Created : 2011-11-14
%%% -------------------------------------------------------------------
-module(tcp_client).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/4]).
-export([control_process/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ----------------------------------------------------------------------
%% Records
%% ----------------------------------------------------------------------
-record(state, {socket,
				peer_ip,
				peer_port,
				fsm_module = built_connections_fsm,
			    info_hash,
                my_peer_id}).


%% ====================================================================
%% Server  start functions
%% ====================================================================

start_link(Host,Port,Info_hash,MyPeer_id)->
	State=#state{peer_ip=Host,peer_port=Port,info_hash=Info_hash,my_peer_id=MyPeer_id},
	gen_server:start_link(?MODULE, State, []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% Connect the peer.
%% --------------------------------------------------------------------
init(#state{peer_ip=Host,peer_port=Port,info_hash=Info_hash,my_peer_id=MyPeer_id} = State) ->
	
	process_flag(trap_exit,true),
	
	case gen_tcp:connect(Host, Port, [binary,{packet,2},{active,false}]) of
		{ok,Socket}->
			{ok,control_process(State#state{socket=Socket,info_hash=Info_hash,my_peer_id=MyPeer_id})};
		{error,Reason}->
			{stop,{connection_error,Reason}}
	end.

%% -----------------------------------------------------------------------
%% Control process for the socket
%% Return the initial state
%% -----------------------------------------------------------------------
control_process(#state{socket= Socket,
					   peer_ip = Ip,
					   peer_port = Port,
					   fsm_module = Module,
					   info_hash = Info_hash,
					   my_peer_id = MyPeer_id} = State)->
	
	{ok,Pid}= client_sup:start_fsm(), %% start gen_fsm process 
	
	gen_tcp:controlling_process(Socket, Pid),     %% And make it the owner process of the socket
	peer_table:add_peer(Pid, Ip, Port, Info_hash), %% Add the peer to the connected table.
	
	Module:start_handshake(Pid,Socket,Info_hash,MyPeer_id), %% Start connection and the handshake
	
	State.


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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

