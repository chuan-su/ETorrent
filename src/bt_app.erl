-module(bt_app).
-behaviour(application).
-export([start/2,stop/1]).

start(_Type,StartArgs)->
    torrent_sup:start_link(StartArgs).
stop(_State)->
    ok.
