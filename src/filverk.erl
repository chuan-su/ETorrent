%@author Alexander
% [read/1, write/2, join/1, split/1, hash/1]
%@author Felix
% [decode/1, reverse/1]
%@author Yuwei Chen
% [encode/1]
%@since 2011/0927
-module(filverk).
-export([read/1, write/2, join/1, split/1, hash/1,encode/1, decode/1]).

%Acharya, Sudarshan 2011 <http://sacharya.com/md5-in-erlang/>

%% ----------------------------------------------------------------------
%% Get the SHA1 value from an encoded iolist
%% ----------------------------------------------------------------------
hash(List)->
crypto:sha(iolist_to_binary(List)).
	
read(Name) ->
	{ok, Bin} = file:read_file(Name),
	Bin.
write(Binary, Name) ->
	file:write_file(Name, Binary).
	
split(Binary) ->
	split(Binary, []).
split(<<BinaryHalf:512, Binary/binary>>, BinList) ->
	split(Binary, [BinaryHalf|BinList]);
split(<<Binary/binary>>, BinList) ->
	[Binary|BinList].

join(Binaries) ->
	join(Binaries, []).
join([], Binary) ->
	list_to_binary(Binary);
join([Head|Binaries], Binary) ->
	join(Binaries, [Head|Binary]).
file(Name) ->
	{ok, Ri} = file:read_file_info(Name),
	Ri.

%% -------------------------------------------------------------------
%% Encode Functions for getting the info hash
%% -------------------------------------------------------------------

encode(N) when is_integer(N) -> 
	["i", integer_to_list(N), "e"];            %% encode integer
encode(B) when is_binary(B) -> 
	[integer_to_list(byte_size(B)), ":", B];   %% encode binary

encode([{B,_}|_] = List) when is_binary(B) ->  %% encode dictionary acturally
	
	Sorted_List = lists:keysort(1, List),  
	["d", [[encode(Key), encode(Value)] || {Key,Value} <- Sorted_List], "e"];

encode(L) when is_list(L) -> ["l", [encode(I) || I <- L], "e"]; %% encode List

encode(Dic)->                                  %% the decode function returns a dictionary
	encode(dict:to_list(Dic)).                 %% Covert dictionary to list 

%% -------------------------------------------------------------------------
%% 	Decode function, I think it is pretty good.
%% -------------------------------------------------------------------------

decode(<<$l, Tail/binary>>) ->
	decode_list(Tail, []);
decode(<<$d,Tail/binary>>)->
    decode_dict(Tail,dict:new());
decode(<<$i,Tail/binary>>) ->
    decode_int(Tail,[]);
decode(Data) ->
   decode_string(Data,[]).

decode_list(Data,Acc)->
	<<List,Tail/binary>>=Data,
       if
	 List==$e -> 
		{reverse(Acc), Tail};
	    List /=$e ->
			{Rest,Left}=decode(Data),
			decode_list(Left,[Rest|Acc])
	end.



decode_dict(<<$e>>,Acc)->
	Acc;
decode_dict(Data,Acc)->     
	<<X,Tail/binary>>=Data,
 
         if
	 X==$e ->            
		{Acc, Tail};
		X/=$e ->
			{Key, Tail1} = decode(Data),
			{Value, Tail2} = decode(Tail1),
			decode_dict(Tail2, dict:store(Key,Value,Acc))
	  end.

decode_int(<<X,Tail/binary>>,Acc)->
        if
	 X==$e -> {list_to_integer(reverse(Acc)),Tail};

	  X/=$e -> decode_int(Tail, [X|Acc])
	end.

	
decode_string(<<X,Rest/binary>>,Acc)->
    if
      X==$: -> Integer=list_to_integer(reverse(Acc)),
	     <<String:Integer/binary, Tail/binary>> = Rest,
		 {String, Tail};

	     X/=$ -> decode_string(Rest,[X|Acc])
     end.

reverse([])->
    [];
reverse([H|T]) ->
 reverse(T)++[H].
