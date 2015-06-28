%% Author: Cesar Kisangani MAKOMBE
%% Personnummer: 730402-5278
%% Created: Nov 23, 2011
%% Description: TODO: Add description to gui
%% This gui module is created in a simple way in wxErlang to let the user of BitTorrent interact with the system
%% without difficulties. The code written below is inspired by the book Erlang (Francesco Cesarini 
%% and the www.erlang.org documentation)
%% I tried to write this first in GS but the teacher discouraged me to do so because the GS module will be removed soon in
%% the next release of Erlang runtime, and I was obliged to begin again. This slowed me down a bit. The code is commented att the end of this
%% module. 
%%

-module(gui).

%%
%% Include files
%%

-include_lib("wx/include/wx.hrl").


%%
%% Exported Functions
%%
-export([start/0]).
%%-compile(export_all).

%% initialize the graphical system


start() ->
	application:load(bitTorrent),
	application:start(bitTorrent),
    Status=create_window(),
   	loop(Status),
    wx:destroy(),
    ok.
%% Main frame

create_window() ->
	Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "BitTorrent_SuccessGroup", [{size, {755,500}}]),
	
%%  Button Panel
	
	Panel = wxPanel:new(Frame,[{size,{755,75}}, {pos,{0,360}}]),
	wxPanel:setBackgroundColour(Panel,?wxBLACK),
	
	%EditorPannel = wxPanel:new(Frame,[{size,{264,351}},{pos,{470,5}}]),
		
	%% Create background of the Frame
	
	Image  = wxBitmap:new("Background.jpg",[{type,?wxBITMAP_TYPE_JPEG}]),
				wxStaticBitmap:new(Frame,?wxID_ANY,Image),
		
	%% Buttons
	

	StatisticButton = wxButton:new(Frame, 1, [{label,"UPDATE STATISTICS"},
				   {size, {200, 50}},{pos,{500,300}}]),
	ChooseTorrentButton = wxButton:new(Panel, 2 , [{label,"choose.TORRENT_file"},
				   {size, {305, 35}},{pos,{10,15}}]),
	DownloadButton = wxButton:new(Panel, 3, [{label,"DOWNLOAD"},
				   {size, {100, 35}},{pos,{320,15}}]),
	UploadButton = wxButton:new(Panel, 4, [{label,"UPLOAD"},
				   {size, {100, 35}},{pos,{425,15}}]),
	Start_PauseButton = wxButton:new(Panel, 5, [{label,"  >    ||  "},
				   {size, {100, 35}},{pos,{530,15}}]),
	QuitButton = wxButton:new(Panel, 6, [{label,"QUIT"},
				   {size, {100, 35}},{pos,{635,15}}]),
	
	Header = wxStaticText:new(Frame, 2000, "----------------STATISTICS----------------",[{pos,{525,10}}]),
	Statistic1 = wxStaticText:new(Frame, 2001, "Total size of the file:------------->",[{pos,{475,50}}]),
	Statistic11 = wxStaticText:new(Frame, 20011, "N/A",[{pos,{650,50}}]),
	Statistic2 = wxStaticText:new(Frame, 2002, "Total size downloaded:----------->",[{pos,{475,100}}]),
	Statistic21 = wxStaticText:new(Frame, 20021, "N/A",[{pos,{650,100}}]),
	Statistic3 = wxStaticText:new(Frame, 2003, "Left size to download:------------>",[{pos,{475,150}}]),
	Statistic31 = wxStaticText:new(Frame, 20031, "N/A",[{pos,{650,150}}]),
	Statistic4 = wxStaticText:new(Frame, 2004, "size downloaded in percentage:->",[{pos,{475,200}}]),
	Statistic40 = wxStaticText:new(Frame, 20040, "%",[{pos,{700,250}}]),
	Statistic41 = wxStaticText:new(Frame, 20041, "N/A",[{pos,{500,250}}]),
	
	
	
	%% gauge with a range of 100%, not connected yet, it is a dead code
	
	Range = 100,
	NormalGauge = wxGauge:new(Frame, 1, Range , [{size, {310, -1}},{pos,{110,345}},
						{style, ?wxGA_HORIZONTAL}]),
	
	%% Menu bar
	
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),
	wxFrame:connect(Frame,command_button_clicked),
	
    MenuBar  = wxMenuBar:new(),
    FileM    = wxMenu:new([]),
    HelpM    = wxMenu:new([]),

  
    wxMenu:append(FileM, ?wxID_EXIT, "&Quit"),
    
    wxMenu:append(HelpM, ?wxID_ABOUT, "&About...\tF1"),

    wxMenu:appendSeparator(HelpM),    
    
    ok = wxFrame:connect(Frame, command_menu_selected), 
	
    wxMenuBar:append(MenuBar, FileM, "&File"),
    wxMenuBar:append(MenuBar, HelpM, "&Help"),
    wxFrame:setMenuBar(Frame, MenuBar),

    ok = wxFrame:setStatusText(Frame, "Welcome to BitTorrent, written in Erlang!",[]),
	 wxFrame:show(Frame),
    {Frame,Statistic1,Statistic11,Statistic2,Statistic21,Statistic3,Statistic31,Statistic41}.

loop(Status) ->
	{Frame,Statistic1,Statistic11,Statistic2,Statistic21,Statistic3,Statistic31,Statistic41}=Status,
	
    receive 
		
		%% Close event
		
  	#wx{event=#wxClose{}} ->
  	    io:format("~p Closing window ~n",[self()]),
  	    wxFrame:destroy(Status),
  	    ok;
		
		%% Menu event
		
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    wxWindow:destroy(Status),
	    ok;
		
		%% Different buttons events
		
	#wx{id=1, event = #wxCommand{type = command_button_clicked}} -> 
		io:format("Statistic button clicked:~n"),
		A = torrent_info:fetch_length(),
		B = torrent_download_records:get_download_amount(),
		C = ((A/B)*100),
		
		wxStaticText:setLabel(Statistic11,integer_to_list(A)),
		wxStaticText:setLabel(Statistic21,integer_to_list(B)),
		wxStaticText:setLabel(Statistic31,integer_to_list(torrent_download_records:get_left_amount())),
		wxStaticText:setLabel(Statistic41,float_to_list(C)),
		loop(Status);
																	
	#wx{id=2, event=#wxCommand{type=command_button_clicked}} ->
		io:format(".torrent button clicked:~n"), 
		File = wxFileDialog:new(Frame,[{style,?wxFD_OPEN}]),
		case wxDialog:showModal(File) of
				?wxID_OK ->	 
					Path = wxFileDialog:getPath(File),
			        Filename = filename:basename(Path),
					bt_app:start(normal, Filename),
					%%torrent_sup:start_link(Filename),
					
					loop(Status);
			
				_-> io:format(" .torrent error:~n"),
					loop(Status)
		end;
	#wx{id=3, event = #wxCommand{type = command_button_clicked}} -> 
		io:format("DownloadButton clicked:~n"),
		torrent_sup:start_torrent(),
     	 loop(Status);
	#wx{id=4, event = #wxCommand{type = command_button_clicked}} -> 
		io:format("UploadButton clicked:~n"),
     	 loop(Status);
	#wx{id=5, event = #wxCommand{type = command_button_clicked}} -> 
		io:format("Start/PauseButton clicked:~n"),
		loop(Status);
	#wx{id=6, event = #wxCommand{type = command_button_clicked}} -> 
		io:format("QuitButton clicked:~n"),
     	  wxFrame:destroy(Frame),
  	    ok;
				
	#wx{id=?wxID_ABOUT, event=#wxCommand{type=command_menu_selected}} ->
	    io:format("Got about ~n", []),
	    dialog(?wxID_ABOUT, Frame),
	    loop(Status);
	Msg ->
	    io:format("Got ~p ~n", [Msg]),
	    loop(Status)
    end.

dialog(?wxID_ABOUT,  Frame) ->
    Str = string:join(["This is BitTorrent, a software which allows you", 
		       "To download document, books... in a faster way using PeerToPeer\n",
			   " For more information about BitTorrent, please follow the link below:\n",
			   " http://en.wikipedia.org/wiki/BitTorrent_(protocol)\n",
			   " For more information and help about this BitTorrent and SuccessGroup",
			   " please contact the Gothenburg University, IT university, CST at www.gu.se. ",     
		       "."], 
		      ""),
    MD = wxMessageDialog:new(Frame,
   			     Str,
   			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
   			      {caption, "About BitTorrent Project"}]),

    wxDialog:showModal(MD),
    wxDialog:destroy(MD).



