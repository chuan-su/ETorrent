%% Author: Flex
%% Created: 2011-12-5
%% Description: TODO: Add description to bitfield
-module(bitfield).


-export([convert/1]).

convert(Number)->

    convert(Number,[]).




convert(Number,List)->

    if 

	 Number div 2 == 0 -> 
 
	   R = Number rem 2,

	    bitchange([R|List]);

	 true ->
 
	   R = Number rem 2,

	    D = Number div 2,

	    convert(D,[R|List])
 
    end.


bitchange(L)->

    case length(L) of

	 1 ->

	    lists:append([0,0,0,0,0,0,0],L);
 
	2 ->

	    lists:append([0,0,0,0,0,0],L);

	 3 ->
 
	   lists:append([0,0,0,0,0],L);

	 4 ->

	    lists:append([0,0,0,0],L);
 
	5 ->

	    lists:append([0,0,0],L);

	 6 ->
 
	   lists:append([0,0],L);

	 7 ->

	    lists:append([0],L);
 
	8 ->

	    L

end.

  %% to run this code u say

  %%  bitfield:convert(255).   you can convert any number to bits, you can convert 248,5,8,......
 









