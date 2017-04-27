-module(iso8583_erl).

%% API exports
-export([]).

%%====================================================================
%% API functions
%%====================================================================


%%for unpacking messages of various types
%%%
-spec unpack(Mformat,InterfaceType,IsoMessage) ->ok|{error,any()} when
	Mformat  		:: list,
	InterfaceType  	:: post,
	IsoMessage		:: [integer()].	
unpack(Mformat,InterfaceType,IsoMessage)-> 
		iso8583_ascii:unpack({Mformat,InterfaceType,IsoMessage}).



%%====================================================================
%% Internal functions
%%====================================================================
