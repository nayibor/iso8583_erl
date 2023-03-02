-module(iso8583_erl).

%% API exports

-export([unpack/2,pack/2,get_field/2,set_field/3,set_field_list/1,set_mti/2,process_data_element/4,
		 create_bitmap/2,get_bitmap_subs/3,get_size_send/2,load_specification/1,get_spec_field/2,get_bitmap_type/1,
		 load_specification_mti/1,get_spec_mti/3,check_mandatory_fields/2,add_echo_fields/3
		 ]).

%%====================================================================
%% API functions
%%====================================================================


%% @doc creats a new map specification which contains the various data elements and a bitmap from a specification file
-spec load_specification(string() |binary())->map().
load_specification(Filename)->
	iso8583_process:load_specification(Filename).

%% @doc this is for loading field information for mtis
-spec load_specification_mti(string() |binary())->map().
load_specification_mti(Filename)->
	iso8583_process:load_specification_mti(Filename).


%% @doc gets the specification for a particular field
-spec get_spec_field(non_neg_integer(),map())->tuple().
get_spec_field(Field,Specification)->
	iso8583_process:get_spec_field(Field,Specification).


%% @doc gets the bitmap type
-spec get_bitmap_type(map())->hex|binary.
get_bitmap_type(Specification)->
	iso8583_process:get_bitmap_type(Specification).


%% @doc for checking mandatory fields
-spec check_mandatory_fields(list(),map())->true|false.
check_mandatory_fields(List_mandatory_keys,Map_fields)->
	iso8583_process:check_mandatory_fields(List_mandatory_keys,Map_fields).


%% @doc for adding fields which are supposed to be echoed
-spec add_echo_fields(map(),map(),map())->map().
add_echo_fields(Map_transaction,Map_recipient,Specification_mti)->
	iso8583_process:add_echo_fields(Map_transaction,Map_recipient,Specification_mti).


%% @doc for getting various specification types
-spec get_spec_mti(atom(),binary(),map())->list()|error.
get_spec_mti(Spec_type,Mti,Spec_field_map)->
	iso8583_process:get_spec_mti(Spec_type,Mti,Spec_field_map).


%% @doc for packing messages into iso list format
-spec pack(Map_pack,Specification)->list()when
	Map_pack ::map(),
	Specification :: map().
pack(Map_pack,Specification)->
	iso8583_process:pack(Map_pack,Specification).


%% @doc for unpacking messages
-spec unpack(IsoMessage,Specification) ->map() when
	IsoMessage		:: [integer()],
	Specification  	:: map().

unpack(IsoMessage,Specification)-> 
	iso8583_process:unpack(IsoMessage,Specification).


%% @doc this is for getting a particular field in an iso message back
-spec get_field(Fld_num::pos_integer()|binary(),Iso_Map::map())->{ok,term()}|error.
get_field(Fld_num,Iso_Map)->
	iso8583_process:get_field(Fld_num,Iso_Map).


%%  @doc for setting the fields for an iso message
-spec set_field(Iso_Map::map(),Fld_num::pos_integer() ,Fld_val::term())->{ok,map()}.
set_field(Iso_Map,Fld_num,Fld_val)->
	iso8583_process:set_field(Iso_Map,Fld_num,Fld_val).


%%  @doc for setting the fields but accepts a list of fields and returns a map containing output
-spec set_field_list(List::list())->map().
set_field_list(List)->
	iso8583_process:set_field_list(List).


%%  @doc for setting the fields for an iso message
-spec set_mti(Iso_Map::map(),Fld_val::term())->{ok,map()}.
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
-spec get_size_send(iolist(),non_neg_integer())->list(). 
get_size_send(Fields_iolist,Length_max_size)->
	iso8583_process:get_size_send(Fields_iolist,Length_max_size).
%%====================================================================
%% Internal functions
%%====================================================================
