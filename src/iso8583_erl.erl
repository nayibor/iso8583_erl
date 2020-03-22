-module(iso8583_erl).

%% API exports
-export([unpack/2,pack/2,get_size/2,set_field/3,set_field_list/1,set_mti/2,process_data_element/4,validate_data/3,
		 create_bitmap/2,get_bitmap_subs/3,get_size_send/3,load_specification/1,get_spec_field/2,get_bitmap_type/1]).

%%====================================================================
%% API functions
%%====================================================================


%% @doc creats a new map specification which contains the various data elements and a bitmap from a specification file
-spec load_specification(string() |binary())->map().
load_specification(Filename)->
	iso8583_process:load_specification(Filename).


%% @doc gets the specification for a particular field
-spec get_spec_field(non_neg_integer(),map())->tuple().
get_spec_field(Field,Specification)->
	iso8583_process:get_spec_field(Field,Specification).


%% @doc gets the bitmap type
-spec get_bitmap_type(map())->hex|binary.
get_bitmap_type(Specification)->
	iso8583_process:get_bitmap_type(Specification).


%% @doc for packing messages into iso list format
-spec pack(Map_pack,Specification)->list()|{error,term()}when
	Map_pack ::map(),
	Specification :: map().
pack(Map_pack,Specification)->
	iso8583_process:pack(Map_pack,Specification).


%% @doc for unpacking messages
-spec unpack(IsoMessage,Specification) ->map()|{error,any()} when
	IsoMessage		:: [integer()],
	Specification  	:: map().

unpack(IsoMessage,Specification)-> 
	iso8583_process:unpack(IsoMessage,Specification).



%% @doc this is for validating an iso8583 message for incoming/outgoing messages based on a  loaded specification
%% specification should have been loaded
%% @doc De_type,Length_field,Fl_vl,Header_length,Format
-spec validate_data(map(),map(),out|in)->{{ok,map()},{error,list()}}.
validate_data(Data_map,Specification_map,Out_or_in)->
	iso8583_validate:validate_data(Data_map,Specification_map,Out_or_in).


%%  @doc for getting the final size of a bitmap or field list
-spec get_size(Type,Value) ->integer() when
	Type    :: bitmap|field_list,
	Value  	:: iolist()|binary()|list().
get_size(Type,Value)->	
	iso8583_process:get_size(Type,Value).


%%  @doc for setting the fields for an iso message
-spec set_field(Iso_Map::map(),Fld_num::pos_integer() ,Fld_val::term())->{ok,map()}|{error,term()}.
set_field(Iso_Map,Fld_num,Fld_val)->
	iso8583_process:set_field(Iso_Map,Fld_num,Fld_val).


%%  @doc for setting the fields but accepts a list of fields and returns a map containing output
-spec set_field_list(List::list())->map().
set_field_list(List)->
	iso8583_process:set_field_list(List).


%%  @doc for setting the fields for an iso message
-spec set_mti(Iso_Map::map(),Fld_val::term())->{ok,map()}|{error,term()}.
set_mti(Iso_Map,Mti_val)->
	iso8583_process:set_mti(Iso_Map,Mti_val).
	


%% @doc for processing the message given the bitmap and the binary containing the data elements
-spec process_data_element(binary(),integer(),binary(),map())->map().
process_data_element(Bitmap,Index_start,Data_binary,Specification)->
	iso8583_process:process_data_element(Bitmap,Index_start,Data_binary,Specification).


%%for creating the final bitmap
%%this bitmap is an 8/16 byte binary with each byte being represented  by an integer.
%%integer converted to a an 2 bit binary represents presence or absence of those fields
-spec create_bitmap(binary|hex,binary())->binary()|list().
create_bitmap(Type_bitmap,Bitmap_final_bit)->
	iso8583_process:create_bitmap(Type_bitmap,Bitmap_final_bit).



%% @doc forr getting the bitmap,mti,Data fields 
-spec get_bitmap_subs(atom(),binary(),map())-> tuple().
get_bitmap_subs(Bitmap_type,Bin_message,Specification)->
	iso8583_process:get_bitmap_subs(Bitmap_type,Bin_message,Specification).



%%for getting the final size of the message to be sent 
-spec get_size_send(binary(),binary()|list(),list())->non_neg_integer().
get_size_send(Mti,Bitmap_final_bit,Fields_list)->
	iso8583_process:get_size_send(Mti,Bitmap_final_bit,Fields_list).
%%====================================================================
%% Internal functions
%%====================================================================
