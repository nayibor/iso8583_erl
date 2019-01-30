-module(iso8583_erl).

%% API exports
-export([unpack/2,pack/2,get_size/2]).

%%====================================================================
%% API functions
%%====================================================================




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



%%  @doc for getting the final size of a bitmap or field list
-spec get_size(Type,Value) ->integer() when
	Type    :: bitmap|field_list,
	Value  	:: iolist()|binary()|list().

get_size(Type,Value)->	
	iso8583_ascii:get_size(Type,Value).



%%====================================================================
%% Internal functions
%%====================================================================
