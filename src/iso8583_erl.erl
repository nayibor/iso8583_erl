-module(iso8583_erl).

%% API exports
-export([unpack/2,pack/2]).

%%====================================================================
%% API functions
%%====================================================================


<<<<<<< HEAD
%% @doc for unpacking messages

-spec unpack(Mformat,Module_process,IsoMessage) ->map()|{error,any()} when
	Mformat  			:: list|binary,
	Module_process  	:: atom,
	IsoMessage			:: [integer()].	
unpack(Mformat,Module_process,IsoMessage)-> 
		iso8583_ascii:unpack(Mformat,Module_process,IsoMessage).
=======
>>>>>>> develop


%% @doc for packing messages into iso list format
-spec pack(Map_pack,Module_process)->list()|{error,term()}when
	Map_pack ::map(),
	Module_process :: atom.
pack(Map_pack,Module_process)->
	iso8583_ascii:pack(Map_pack,Module_process).


%% @doc for unpacking messages
-spec unpack(IsoMessage,Module_process) ->map()|{error,any()} when
	IsoMessage			:: [integer()],
	Module_process  	:: atom.

unpack(IsoMessage,Module_process)-> 
	iso8583_ascii:unpack(IsoMessage,Module_process).




%%====================================================================
%% Internal functions
%%====================================================================
