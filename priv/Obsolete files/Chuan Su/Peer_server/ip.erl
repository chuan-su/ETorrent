%% Author: chuan su
%% Created: 2011-11-10
%% Description: TODO: Add description to ip
-module(ip).

%%
%% Exported Functions
%%
-export([get_ip_address/0]).

%%
%% Get the IP address which is not local host
%%

get_ip_address()->
	{ok,Ip_triples}=inet:getiflist(),
	Ip_list=lists:filter(fun is_not_localhost/1, Ip_triples),
	{ok,Ip}=inet_parse:address(get_element(Ip_list)),
	Ip.
	

%%
%% Local Functions
%%

is_not_localhost("127.0.0.1")-> false;
is_not_localhost(_IP)-> true.

get_element([Head|_])->
	Head.