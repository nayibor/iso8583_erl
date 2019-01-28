-module(iso8583_erl).

%% API exports
-export([unpack/3,pack/2]).

%%====================================================================
%% API functions
%%====================================================================


%% @doc for unpacking messages

-spec unpack(Mformat,Module_process,IsoMessage) ->map()|{error,any()} when
	Mformat  			:: list|binary,
	Module_process  	:: atom,
	IsoMessage			:: [integer()].	
unpack(Mformat,Module_process,IsoMessage)-> 
		iso8583_ascii:unpack(Mformat,Module_process,IsoMessage).


%% @doc for packing messages into iso list format
-spec pack(Map_pack,Module_process)->list()|{error,term()}when
	Map_pack ::map(),
	Module_process :: atom.
pack(Map_pack,Module_process)->
	iso8583_ascii:pack(Map_pack,Module_process).

%%====================================================================
%% Internal functions
%%====================================================================
