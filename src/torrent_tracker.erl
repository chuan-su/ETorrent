%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
%%% Description : communicate with Tracker 
%%%               Specify the http request and send request and handle
%%%               Response from tracker.
%%% Created : 2011-11-24
%%% -------------------------------------------------------------------
-module(torrent_tracker).

-behaviour(gen_fsm).

%% --------------------------------------------------------------------
%% Export four states
-export([init_tracker_request/2,send_request/2,handle_tracker_response/2,
		 send_torrent_info/2,peer_to_peer/2]).

%% Export start function
-export([start_link/0,start_connection/0]).

%% gen_fsm callbacks
-export([init/1,  state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% Records of http request parameters , not include the announce url
-record(request_parameters,{ info_hash,
						     peer_id,
						     port,
						     uploaded,
						     downloaded,
						     left,
						     compact,
						     event}).

%% Records of tracker response parameters
-record(tracker_response, {warning_message = null, %% default null
						   internal,
						   tracker_id = null,      %% default null
						   complete,
						   incomplet,
						   peers}).

%% Records of tracker state ,contains the request and response records

-record(tracker_state, {url,                       %% announce url
                        tracker_request,           %% request parameters records
						response,                  %% response message not handled,raw data
						http_request,              %% http request url
						tracker_response           %% response parameters records
						}).
-define(TIMEOUT,0).


%% ====================================================================
%% External functions
%% ====================================================================


%% -------------------------------------------------------------------
%% generate the event for requesting
%% -------------------------------------------------------------------
event(completed,0) ->
	completed;
event(_Download,_Left) ->
	started.

%% --------------------------------------------------------------------
%% Correct value format function
%% For gererate the requested format of http request url
%% --------------------------------------------------------------------

parameter_check(Integer) when is_integer(Integer)->
	integer_to_list(Integer);
parameter_check(Atom) when is_atom(Atom)->
	atom_to_list(Atom);
parameter_check(List) when is_list(List)->
	List.

%% ------------------------------------------------------------------------
%% check if we have tracker id ,if have add the tracker id to the request url
%% -------------------------------------------------------------------------
parameter_list(null,Parameters)->
	Parameters;

parameter_list(Tracker_id,Parameters) ->
	T = {"trackerid",parameter_check(Tracker_id)},
	lists:append(Parameters, [T]).

string_handler(List)->
	Sequences=[string:join([Key,Value], "=")||{Key,Value} <- List],
	string:join(Sequences, "&"). 

%% -------------------------------------------------------------------------
%% Url encode function 
%% Get the requested ure encoded info_hash
%% -------------------------------------------------------------------------

url_encode(Binary)when is_binary(Binary) ->
	url_encode(binary_to_list(Binary));

url_encode(List)  when is_list(List)->
	lists:flatten([encode(Byte)||Byte <- List]).

%% encoded using the "%nn" format, where nn is the hexadecimal value of the byte.
encode(Byte)->
    Format=encode_format(),
	case lists:member(Byte, Format) of %% check whether the byte is in that scape
		true ->
			Byte;
		false ->
			lists:concat(["%",io_lib:format("~2.16.0B", [Byte])])
	end.

%% generate a list which is the requested scape for each byte	
encode_format()->
				   
		List1=lists:seq($0,$9),
		List2=lists:seq($a,$z),
		List3=lists:seq($A,$Z),
		List4=[$.,$~,$-,$_],
		List1++List2++List3++List4.

%% Simply specify my peer_id	
my_id()->
    Id = "-FT-1234567890123456",
	Id.

%% ----------------------------------------------------------------------
%% Tracker response handler functions 
%% Retreive each value from the response
%% ----------------------------------------------------------------------

%% get the warning message if it exits
get_warning_message(Dict)->
	
	try dict:fetch(<<"warning message">>,Dict) of
		Warning when is_list(Warning) ->
			Warning           %% return warning message
	catch
		error:_ ->
			no_warning_message %% return no_warning_message if no warning //atom
	end.

%% get the internal between requests
get_internal(Dict)->
	dict:fetch(<<"interval">>, Dict).

%% get tracker id if it exits or return null
get_tracker_id(Dict)->
	try dict:fetch(<<"tracker id">>,Dict) of
		Tracker_id when is_list(Tracker_id) ->
			Tracker_id
	catch
		error:_ ->
			null              %% return null //atom
	end.

%% get seeders ammount
get_seeders_number(Dict)->
	dict:fetch(<<"complete">>, Dict).

%% get incomplete ammount 
get_leechers_number(Dict)->
	dict:fetch(<<"incomplete">>, Dict).


%% ---------------------------------------------------------
%% get peers information,and decode the peers of two models
%% ---------------------------------------------------------
get_peers(Dict)->
	case dict:fetch(<<"peers">>,Dict) of 
		
		Binary when is_binary(Binary) ->  %% binary model
			decode_peers_binary(Binary,[]);
		
		List   when is_list(List)     ->  %% dictionary model
			decode_peers_dict(List,[])
	end.

%% ---------------------------------------------------------		
%% decode the  peers in bianry model
%% ---------------------------------------------------------
decode_peers_binary(<<IP1:8,IP2:8,IP3:8,IP4:8,Port:16/big,Rest/binary>>,Acc)->

    decode_peers_binary(Rest,[{{IP1,IP2,IP3,IP4},Port}|Acc]);

decode_peers_binary(<<>>,Acc)->
 L = lists:reverse(Acc),
 io:format("~p", [L]),
	L.

%% -----------------------------------------------------------
%% decode the  peers in list of dictionary model
%% -----------------------------------------------------------
decode_peers_dict([Dict|Rest],Acc)->
	
	IP = dict:fetch(<<"ip">>,Dict),        %% ignore the peer id
	PORT = dict:fetch(<<"port">>,Dict),
    decode_peers_dict(Rest,[{binary_to_list(IP),PORT}|Acc]);

decode_peers_dict([],Acc)->
	L = lists:reverse(Acc),
	io:format("~p", [L]),
	L.

%% ====================================================================
%% Start gen_fsm functions
%% ====================================================================

start_link()->
	Tracker_state = #tracker_state{tracker_request = #request_parameters{},
								   tracker_response = #tracker_response{}},
	gen_fsm:start_link({local,?MODULE}, ?MODULE, Tracker_state, []).


start_connection()->
	gen_fsm:send_event(?MODULE, start_communication).

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, init_tracker_request, StateData}          |
%% Initial request parameters with uploaded=0 ,downloaded =0,event=started
%% --------------------------------------------------------------------

init(Tracker_state) ->
	
	process_flag(trap_exit,true),
	
	Url         =torrent_info:fetch_announce(),
	Info_hash   =torrent_info:fetch_info_hash(),
	Downloaded  = torrent_download_records:get_download_amount(), %% get download amount from recorder
	Uploaded    = torrent_download_records:get_uploaded_amount(), %% get upload amount from recorder
	Left        = torrent_download_records:get_left_amount(),     %% get left amount from recorder
	My_Id       = my_id(),
	Event       = event(Downloaded,Left),  %% if Left ==0 then the event will be completed

	Tracker_State = Tracker_state#tracker_state{url =Url,
												tracker_request =
											    #request_parameters{info_hash   = Info_hash,
								                                     peer_id    = My_Id,
								                                     port       = 2710,
								                                     uploaded   = Uploaded ,
								                                     downloaded = Downloaded,
								                                     left       = Left,
								                                     compact    =1,
								                                     event      = Event}},
	inets:start(),
    {ok, init_tracker_request, Tracker_State}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% State 1 : init_tracker_request/2
%% Returns : {next_state,send_request,StateData,Timeout}
%% Initial the http request ,generate the http request url
%% ---------------------------------------------------------------------

init_tracker_request(start_communication, #tracker_state{url =Url,
					  tracker_request=#request_parameters{info_hash  = Info_hash,
											               peer_id    =  My_id,
											               port       =  Port,	
											               uploaded   =  Uploaded,
											               downloaded =  Downloaded,
											               left       =  Left,											
											               compact    =  Compact,
											               event      =  Event}} = StateData)->
	Parameters=[
				 {"info_hash",url_encode(Info_hash)},
			     {"peer_id",   My_id},
				 {"port",      parameter_check(Port)},
				 {"uploaded",  parameter_check(Uploaded)},
				 {"downloaded",parameter_check(Downloaded)},
			     {"left",      parameter_check(Left)},				
				 {"compact",   parameter_check(Compact)},
				 {"event",     parameter_check(Event)}],
	
	Requst_Parameters=string_handler(Parameters), %% String handler , make the string requested formate.
	
    Http_request = lists:concat([Url,"?",Requst_Parameters]),
	

	{next_state,send_request,StateData#tracker_state{http_request = Http_request},?TIMEOUT};

%% -----------------------------------------------------------------------------------
%% when connecting to tracker again, after "internal" time
%% this state will be invoked automatically 
%% -----------------------------------------------------------------------------------

init_tracker_request(timeout,#tracker_state{url = Url,
					   tracker_request=#request_parameters{info_hash  = Info_hash,
											               peer_id    =  My_id,
											               port       =  Port,						
											               compact    =  Compact},
						tracker_response = #tracker_response{tracker_id = Id}= StateData}) ->

      Downloaded  = torrent_download_records:get_download_amount(),
	  Uploaded    = torrent_download_records:get_uploaded_amount(),
	  Left        = torrent_download_records:get_left_amount(),
      Event       = event(Downloaded,Left),
      
      Parameters=[
				 {"info_hash",url_encode(Info_hash)},
			     {"peer_id",   My_id},
				 {"port",      parameter_check(Port)},
				 {"uploaded",  parameter_check(Uploaded)},
				 {"downloaded",parameter_check(Downloaded)},
			     {"left",      parameter_check(Left)},				
				 {"compact",   parameter_check(Compact)},
				 {"event",     parameter_check(Event)}],
	  
	P = parameter_list(Id,Parameters),
	Requst_Parameters=string_handler(P),
    Http_request = lists:concat([Url,"?",Requst_Parameters]),
	  
{next_state,send_request,StateData#tracker_state{http_request = Http_request},?TIMEOUT}.


%% ------------------------------------------------------------------------
%% State 2 : send_request/2
%% Returns : {next_state,handle_tracker_response,StateData,Timeout}
%% Send http request to the tracker and get the response
%% -------------------------------------------------------------------------

send_request(timeout,#tracker_state{http_request = URL} = StateData)->
	
	case httpc:request(get, {URL, []}, [], []) of
		
		{ok,Response}->
			
	
		{next_state,handle_tracker_response,StateData#tracker_state{response = Response},?TIMEOUT};
	
		{error,Error}->

			{stop,Error,StateData}
	end.

%% --------------------------------------------------------------------------
%% State 3 : handle_tracker_response/2
%% Return  : {next_state,save_to_database,StateData,Timeout}
%% Handle the response from tracker,get each value of the response
%% ---------------------------------------------------------------------------

handle_tracker_response(timeout,#tracker_state{response = Response}=StateData) ->
	
	 case Response of
		 {{_,200,_},_,Body}-> 
			         Dict = filverk:decode(list_to_binary(Body)),

           Tracker_Response = #tracker_response{
                              warning_message = get_warning_message(Dict),
						      internal        = get_internal(Dict),
						      tracker_id      = get_tracker_id(Dict),
						      complete        = get_seeders_number(Dict),
						      incomplet       = get_leechers_number(Dict),
						      peers           = get_peers(Dict)},
				
   {next_state,send_torrent_info,StateData#tracker_state{tracker_response = Tracker_Response},?TIMEOUT}
	 
	 end.

%% ---------------------------------------------------------------------------
%% State 4 : send_torrent_info/2
%% Return  : {next_state,peer_to_peer,StateData,Timeout}
%% Inform connections manager the download file infohash and our id
%% ----------------------------------------------------------------------------


send_torrent_info(timeout,#tracker_state{tracker_request = #request_parameters{info_hash = Info_hash,
																peer_id   = Peer_id}}= StateData) ->
	connections_manager:file_info(Info_hash, Peer_id),
	
	{next_state,peer_to_peer,StateData,?TIMEOUT}.

%% ---------------------------------------------------------------------------
%% State 5 : peer_to_peer/2
%% Return  : {next_state,init,StateData}
%% Start peer to peer connections
%% ----------------------------------------------------------------------------


peer_to_peer(timeout,#tracker_state{tracker_response = 
									#tracker_response{internal  = Internal,
						                              peers     = Peers}} =StateData) ->
	connections_manager:peer_to_peer(Peers),
	
	Internal_TIMEOUT = timer:seconds(Internal),
	io:format("~p", [Internal_TIMEOUT]),
	
%% after "internal" timeout it will send request to the tracker again.
	{next_state,init_tracker_request,StateData,Internal_TIMEOUT}.

 	
	

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name(_Event, _From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
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
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

