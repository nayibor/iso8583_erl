-module(iso8583_erl).

%% API exports
-export([unpack/3]).

%%====================================================================
%% API functions
%%====================================================================


%% @doc for unpacking messages of various types
%%for now api works with only Mformat=list and InterfaceType=post 
%%other ones will be added later 
%%%
-spec unpack(Mformat,InterfaceType,IsoMessage) ->map()|{error,any()} when
	Mformat  		:: list,
	InterfaceType  	:: post,
	IsoMessage		:: [integer()].	
unpack(Mformat,InterfaceType,IsoMessage)-> 
		iso8583_ascii:unpack(Mformat,InterfaceType,IsoMessage).



%%====================================================================
%% Internal functions
%%====================================================================
