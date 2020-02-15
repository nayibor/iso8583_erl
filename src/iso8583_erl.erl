-module(iso8583_erl).

%% API exports
-export([unpack/2,pack/2,get_size/2,set_field/4,set_mti/3,process_data_element/4,create_bitmap/2,get_bitmap_subs/3]).


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


%%  @doc for setting the fields for an iso message
-spec set_field(Iso_Map::map(),Fld_num::pos_integer() ,Fld_val::term(),Module_process::atom)->{ok,map()}|{error,term()}.
set_field(Iso_Map,Fld_num,Fld_val,Module_process)->
	iso8583_ascii:set_field(Iso_Map,Fld_num,Fld_val,Module_process).
	

%%  @doc for setting the fields for an iso message
-spec set_mti(Iso_Map::map(),Fld_val::term(),Module_process::atom)->{ok,map()}|{error,term()}.
set_mti(Iso_Map,Mti_val,Module_process)->
	iso8583_ascii:set_mti(Iso_Map,mti,Mti_val,Module_process).


%% @doc for processing the message given the bitmap and the binary containing the data elements
-spec process_data_element(binary(),integer(),binary(),atom())->map().
process_data_element(Bitmap,Index_start,Data_binary,Module_process)->
	iso8583_ascii:process_data_element(Bitmap,Index_start,Data_binary,Module_process).


%%for creating the final bitmap
%%this bitmap is an 8/16 byte binary with each byte being represented  by an integer.
%%integer converted to a an 2 bit binary represents presence or absence of those fields
-spec create_bitmap(binary|hex,binary())->binary()|list().
create_bitmap(Type_bitmap,Bitmap_final_bit)->
	iso8583_ascii:create_bitmap(Type_bitmap,Bitmap_final_bit).


%% @doc forr getting the bitmap,mti,Data fields 
-spec get_bitmap_subs(atom(),binary(),atom())-> tuple().
get_bitmap_subs(Binary_type,Bin_message,Module_process)->
	iso8583_ascii:get_bitmap_subs(binary,Bin_message,Module_process).
%%====================================================================
%% Internal functions
%%====================================================================
