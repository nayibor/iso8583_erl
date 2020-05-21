%%%
%%% @doc iso8583_erl module.
%%%<br>this module is the api for library </br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>

-module(iso8583_erl).

%% API exports
-export([unpack/2,pack/2,get_size/2,get_field/2,set_field/3,set_field_list/1,set_mti/2,process_data_element/4,
		 create_bitmap/2,get_bitmap_subs/3,get_size_send/2,load_specification/1,get_spec_field/2,get_bitmap_type/1]).

%%====================================================================
%% API functions
%%====================================================================


%% @doc creats a  specification which contains the various data elements and their specifications from a spec file
-spec load_specification(string() |binary())->map().
load_specification(Filename)->
	iso8583_process:load_specification(Filename).


%% @doc gets the specification for a particular field
-spec get_spec_field(non_neg_integer(),map())->tuple().
get_spec_field(Field,Specification)->
	iso8583_process:get_spec_field(Field,Specification).


%% @doc gets the bitmap type from a specification
-spec get_bitmap_type(map())->hex|binary.
get_bitmap_type(Specification)->
	iso8583_process:get_bitmap_type(Specification).


%% @doc for packing messages into an iolist format
-spec pack(Map_pack,Specification)->list()|{error,term()}when
	Map_pack ::map(),
	Specification :: map().
pack(Map_pack,Specification)->
	iso8583_process:pack(Map_pack,Specification).


%% @doc for unpacking messages into a map object
-spec unpack(IsoMessage,Specification) ->map()|{error,any()} when
	IsoMessage		:: [integer()],
	Specification  	:: map().

unpack(IsoMessage,Specification)-> 
	iso8583_process:unpack(IsoMessage,Specification).


%%  @doc for getting the size of a bitmap or an iolist of fields
-spec get_size(Type,Value) ->integer() when
	Type    :: bitmap|field_list,
	Value  	:: iolist()|binary()|list().
get_size(Type,Value)->	
	iso8583_process:get_size(Type,Value).



%% @doc this is for getting a particular field in an iso message back
-spec get_field(Fld_num::pos_integer()|binary(),Iso_Map::map())->{ok,term()}|error.
get_field(Fld_num,Iso_Map)->
	iso8583_process:get_field(Fld_num,Iso_Map).


%%  @doc for setting the fields for an iso message
-spec set_field(Iso_Map::map(),Fld_num::pos_integer() ,Fld_val::term())->{ok,map()}|{error,term()}.
set_field(Iso_Map,Fld_num,Fld_val)->
	iso8583_process:set_field(Iso_Map,Fld_num,Fld_val).


%%  @doc for setting a list of fields
-spec set_field_list(List::list())->map().
set_field_list(List)->
	iso8583_process:set_field_list(List).


%%  @doc for setting the a single field for an iso message
-spec set_mti(Iso_Map::map(),Fld_val::term())->{ok,map()}|{error,term()}.
set_mti(Iso_Map,Mti_val)->
	iso8583_process:set_mti(Iso_Map,Mti_val).
	


%% @doc for processing the message given the bitmap and the binary containing the data elements
-spec process_data_element(binary(),integer(),binary(),map())->map().
process_data_element(Bitmap,Index_start,Data_binary,Specification)->
	iso8583_process:process_data_element(Bitmap,Index_start,Data_binary,Specification).


%% @doc for creating the bitmap of a message
-spec create_bitmap(binary|hex,binary())->binary()|list().
create_bitmap(Type_bitmap,Bitmap_final_bit)->
	iso8583_process:create_bitmap(Type_bitmap,Bitmap_final_bit).



%% @doc for getting the bitmap,mti,Data fields of an iso message
-spec get_bitmap_subs(atom(),binary(),map())-> tuple().
get_bitmap_subs(Bitmap_type,Bin_message,Specification)->
	iso8583_process:get_bitmap_subs(Bitmap_type,Bin_message,Specification).



%%for getting the size of the message to be sent 
%%-spec get_size_send(binary(),binary()|list(),list())->non_neg_integer().
-spec get_size_send(iolist(),non_neg_integer())->list(). 
get_size_send(Fields_iolist,Length_max_size)->
	Final_length = erlang:iolist_size(Fields_iolist),
	string:right(erlang:integer_to_list(Final_length),Length_max_size,$0).
%%====================================================================
%% Internal functions
%%====================================================================
