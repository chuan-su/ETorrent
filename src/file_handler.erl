%%% -------------------------------------------------------------------
%%% Author  : Chuan Su
-module(file_handler).
-export([open/3]).

open(File_name,Index,Block)->
	
	Piece_length = torrent_info:fetch_piece_length(),
          Position=Index*Piece_length,
    case file:open(File_name,[read,write,binary]) of
         {ok,IoDevice}->
	    file:pwrite(IoDevice,Position,Block);
	{error,Reason} ->
            {error,Reason}
     end.
