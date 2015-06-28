
%% Author Chuan Su
-module(multiple_file_handler_sup).
-behaviour(supervisor).
-export([start_link/0,init/1]).


-define(MAX_RESTART,5).
-define(MAX_TIME,60).
-define(SHUT_DOWN,5000).


start_link()->
       supervisor:start_link({local,?MODULE},?MODULE,[]).

init([])->
	
       SupFlags={one_for_one,?MAX_RESTART,?MAX_TIME},
           Child_Spec={multiple_file_handler,
                         {multiple_file_handler,start_link,[]},
                          permanent,
                          ?SHUT_DOWN,
                          worker,
                          [multiple_file_handler,storage_dir]},
	   
           {ok,{SupFlags,[Child_Spec]}}.
   