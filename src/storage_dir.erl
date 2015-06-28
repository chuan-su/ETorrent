%% Author Chuan Su
-module(storage_dir).
-export([main_folder/1,files/3,make_file/3]).


main_folder(Folder_name)->
     case file:make_dir(Folder_name) of        %% create main folder for files
           ok->
            ok;
           {error,Reason}->
           Reason
     end.

files(Path,Position,Data)->
      case filelib:ensure_dir(Path) of      %% find the dir of files
           ok->
               make_file(Path,Position,Data);
           {error,Reason}->
               Reason
       end.

make_file(Path,Position,Data)->
        case file:open(Path,[read,write,binary]) of   %% open files
              {ok,IoDevice}-> 
     file:pwrite(IoDevice,Position,Data);  %%write binary to files in position
              {error,Reason}->
                  Reason
        end.





