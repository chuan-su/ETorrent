%%% -------------------------------------------------------------------
%%% Author  : chuan su
%%% Description :
%%%
%%% Created : 2011-10-26
%%% -------------------------------------------------------------------
-module(fsm_socket_handler).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

-export([set_client_socket/2,decrease_current_connection/0]).

-export([wait_handshake_state/2,handshake_state/2]). %% gen_fsm states

%% gen_fsm callbacks
-export([init/1, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


-record(connection_states,{am_chocking =1,        %% connection start as chocked, not interested
						   am_interested =0,
						   peer_chocking =1,
						   peer_interested =0
						   }).
-record(state,{client_socket,
			   client_ip,
               client_port,
			   my_peer_id
			   }).
-define(PSTR,"BitTorrent protocol").
-define(TIMEOUT,120000).

%% ====================================================================
%% Start the general FSM by its supervisor
%% ====================================================================
start_link(My_peer_id)->
	

	gen_fsm:start_link(?MODULE, My_peer_id, []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init(My_peer_id) ->
	process_flag(trap_exit,true),
	
	io:format("~s~n", ["fsm process started"]),
  {ok,wait_client_state,#state{my_peer_id=My_peer_id}}.

%% Triggar the init state and start the fsm states.
set_client_socket(Pid,Socket)->
	
	gen_fsm:send_event(Pid, {send_header,Socket}).

decrease_current_connection()->
	gen_server:cast(peer_server, {connection_closed,self()}).

%% --------------------------------------------------------------------
%% Func: wait_client_state/2
%% Returns:  {next_state, handshake_state, NextStateData, Timeout} |
%% --------------------------------------------------------------------
wait_handshake_state({send_header,Socket},State) ->
	
	Header=get_header(),
	
	case gen_tcp:send(Socket, Header) of
		
		 ok ->		 
	inet:setopts(Socket, [{active,once},{packet,2},binary]),
	{ok,{IP,Port}}= inet:peername(Socket),
	io:format("~s~w~n", ["ok,client ip is",IP]),
	
	{next_state,handshake_state,State#state{client_socket = Socket,client_ip=IP,client_port=Port},?TIMEOUT};
		
		{error,_}->
			{stop,handshake_error,State}
	end.

get_header()->
  Pstrlen= length(?PSTR),
  Reserved_byte = <<0:64/big>>,
  <<Pstrlen:8,?PSTR,Reserved_byte/binary>>.

handshake_payload(Socket,Info_hash,My_peer_id)->
	
	try
		ok = gen_tcp:send(Socket,Info_hash),
		ok = gen_tcp:send(Socket,My_peer_id),
		ok
	catch
		error:_ ->
			{error,instance}
	end.

%%---------------------------------------------------------------------
%% Func: handshake_state/2
%% Returns:  {next_state,message_state,StataData,TimeOut}   |
%%           {stop,normal,StateData}                        |
%%---------------------------------------------------------------------

handshake_state({packet,Packet},#state{client_socket=Socket,client_ip=IP,client_port=Port,my_peer_id= My_peer_id}=State)->
	
    case Packet of
  <<Pstrlen:8/integer,?PSTR,_Reserved_Byte:64/big,_Info:20/binary,_Peer_id:20/binary>> 
   when Pstrlen /= length(?PSTR) ->
    {stop,pstrlen_wrong,State};
		
  <<Pstrlen:8/integer,?PSTR,_Reserved_Byte:64/big,Info:20/binary,Peer_id:20/binary>>
                                 ->
	 try
		 ok = check_info_hash(Info),
		 ok = check_peer_id(IP,Port,Peer_id),
		 
		 handshake_payload(Socket,Info,My_peer_id),
		 
		 {next_state,message_state,State}
	 catch
		 error:_ ->
			 {stop,error,State}
    end;
	Header when is_binary(Header) ->
		    {stop,not_handshake,State}
     end;

handshake_state(timeout,State) ->
	{stop,normal,State}.

%% check whether the hash value is current serving

check_info_hash(Info)->
	ok.

check_peer_id(IP,Port,Peer_id)->
	ok.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
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
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

%% Any spacket sent from clients will be handler in handle_info method
handle_info({tcp,Socket,Packet}, StateName, #state{client_socket= Socket}=StateData) ->
	
	io:format("~s~n", ["handle_info triggered"]),
	inet:setopts(Socket, [{active,once}]),                    %% Flow controll : enable to receive next TCP message
	io:format("~s~w~n",["ok,packet is",binary_to_term(Packet)]),
	
	?MODULE:StateName({packet,Packet},StateData);             %% Trigger the current state to handle the message

handle_info({tcp_closed,Socket},_StateName,#state{client_socket = Socket}= StateData)->
	
	{stop,normal,StateData}; %% close the connection

handle_info(_Info,StateName,StateData)->
	
	{noreply, StateName,StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName,#state{client_socket= Socket}) ->
	catch(gen_tcp:close(Socket)),
	decrease_current_connection(), %% inform the gen_server a connection has closed
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

