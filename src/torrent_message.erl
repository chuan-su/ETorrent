%% Author: Chuan Su
%% Created: 2011-12-4
%% Description: decode and encode the message type.
-module(torrent_message).

%%
%% Message type.
%%
-define(CHOCK,0:8).
-define(UNCHOCK,1:8).
-define(INTERESTED,2:8).
-define(UNINTERESTED,3:8).
-define(HAVE,4:8).
-define(BITFIELD,5:8).
-define(REQUSET,6:8).
-define(PIECE,7:8).
-define(CANCLE,8:8).
-define(PORT,9:8).
%%
%% Exported Functions
%%
-export([incoming/1,outgoing/1]).


%%
%% decode the incoming message
%%
incoming(Message) ->
	case Message of 
		<<>> -> keep_alive;
		<<?CHOCK>> -> choke;
		<<?UNCHOCK>> -> unchoke;
		<<?INTERESTED>> -> interested;
		<<?UNINTERESTED>> -> not_interested;
		<<?HAVE,Piece_index:32>> -> {have,Piece_index};
		<<?BITFIELD,BitField/binary>> -> {bitfield,BitField};
		<<?REQUSET,Index:32,Begin:32,Length:32>> -> {request,Index,Begin,Length};
		<<?PIECE, Index:32,Begin:32,Block/binary>> -> {piece,Index,Begin,Block};
		<<?CANCLE,Index:32,Begin:32,Length:32>> -> {cancle,Index,Begin,Length};
		<<?PORT,Port:16>> -> {port,Port}
	end.

%%
%% encode the outgoing message
%%
outgoing(Message) ->
	 case Message of 
		 keep_alive -> <<>>;
		 choke -> <<?CHOCK>>;
		 unchoke -> <<?UNCHOCK>>;
		 interested -> <<?INTERESTED>>;
		 not_interested -> <<?UNINTERESTED>>;
		 {hava,Piece_index} -> <<?HAVE,Piece_index:32>>;
		 {bitfield,BitField} -> <<?BITFIELD,BitField/binary>>;
		 {request,Index,Begin,Length} -> <<?REQUSET,Index:32,Begin:32,Length:32>>;
		 {piece,Index,Begin,Block} -> <<?PIECE, Index:32,Begin:32,Block/binary>>;
		 {cancle,Index,Begin,Length} -> <<?CANCLE,Index:32,Begin:32,Length:32>>;
		 {port,Port} -> <<?PORT,Port:16>>
	 end.


		


