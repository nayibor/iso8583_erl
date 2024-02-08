%%% @doc this module is the api file for the iso8583_erl project
%% @end
%% @author Nuku Ameyibor
-module(iso8583_erl).

%% API exports

-export([unpack/2,pack/2,get_field/2,set_field/3,set_field_list/1,set_mti/2,
		 create_bitmap/2,get_size_send/2,load_specification/1,get_spec_field/2,get_bitmap_type/1,
		 load_specification_mti/1,get_spec_mti/3,check_mandatory_fields/2,add_echo_fields/3
		 ]).

%%====================================================================
%% API functions
%%====================================================================


%% @doc creates a map which contains the various data elements and the bitmap type from a specification file.
%%
%%%will throw an exception if specification file has wrong field information.
%%
%%example below
%%```
%% 70> Spec = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/"++"spec.cfg").
%% #{16 => {4,fx,0,"MMDD",{none,none}},
%%  107 => {999,vl,3,"ANS",{none,none}},
%%  115 => {999,vl,3,"ANS",{none,none}},...}
%% 71>
%%'''
%% @end
%% @throws {binary(),non_neg_integer(),map()}
-spec load_specification(string() |binary())->map().
load_specification(Filename)->
	iso8583_process:load_specification(Filename).


%% @doc this is for loading field information for the message type indicator(mti)
%% @hidden
-spec load_specification_mti(string() |binary())->map().
load_specification_mti(Filename)->
	iso8583_process:load_specification_mti(Filename).


%% @doc gets the specification for a particular field.
%%
%% field number must be between 2 and 128.
%%
%%example below
%%```
%% 119> iso8583_erl:get_spec_field(2,Spec).
%% {19,vl,2,"N",{none,none}}
%% 120>
%%'''
%% @end 
-spec get_spec_field(non_neg_integer(),map())->tuple().
get_spec_field(Field,Specification)->
	iso8583_process:get_spec_field(Field,Specification).


%% @doc gets the bitmap type from the specification.
%%
%% example below
%%```
%% 130> iso8583_erl:get_bitmap_type(Spec).
%% hex
%% 131>
%%''' 
%% @end
-spec get_bitmap_type(map())->hex|binary.
get_bitmap_type(Specification)->
	iso8583_process:get_bitmap_type(Specification).


%% @doc for checking mandatory fields
%% @hidden
-spec check_mandatory_fields(list(),map())->true|false.
check_mandatory_fields(List_mandatory_keys,Map_fields)->
	iso8583_process:check_mandatory_fields(List_mandatory_keys,Map_fields).


%% @doc for adding fields which are supposed to be echoed back to recipient
%% @hidden
-spec add_echo_fields(map(),map(),map())->map().
add_echo_fields(Map_transaction,Map_recipient,Specification_mti)->
	iso8583_process:add_echo_fields(Map_transaction,Map_recipient,Specification_mti).


%% @doc for getting specification info about mti
%% @hidden
-spec get_spec_mti(atom(),map(),map())->list()|error.
get_spec_mti(Spec_type,Mti,Spec_field_map)->
	iso8583_process:get_spec_mti(Spec_type,Mti,Spec_field_map).


%% @doc for packing a map containing iso fields into iolist.
%%
%% example below
%%```
%% 135> {ok,First_map} = iso8583_erl:set_mti(maps:new(),<<"0200">>),
%%  .. {ok,Second_map} = iso8583_erl:set_field(First_map,3,<<"201234">>),
%%  .. {ok,Third_map} = iso8583_erl:set_field(Second_map,4,<<"4.5">>),
%%  .. {ok,Fourth_map} = iso8583_erl:set_field(Third_map,5,<<"5000">>),
%%  .. {ok,Fifth_map} = iso8583_erl:set_field(Fourth_map,102,<<"123413243">>),
%%  .. {ok,Six_map} = iso8583_erl:set_field(Fifth_map,103,<<"12897979987">>),
%%  .. [Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Six_map,Specification).
%% ["0200","B8000000000000000000000006000000",["201234","0000000004.5","000000005000",["09","123413243"],["11","12897979987"]]]
%% 136>
%%'''
%% @end
-spec pack(Map_pack,Specification)->list()when
	Map_pack ::map(),
	Specification :: map().
pack(Map_pack,Specification)->
	iso8583_process:pack(Map_pack,Specification).


%% @doc for unpacking a formatted iso messages from iolist/list into map format.
%%
%% example below
%%```
%% 162> Message.
%% "0200B80000000000000000000000060000002012340000000004.5000000005000091234132431112897979987"
%% 163> iso8583_erl:unpack(Message,Specification).
%% #{3 => <<"201234">>,4 => <<"0000000004.5">>,
%%  5 => <<"000000005000">>,102 => <<"123413243">>,
%%  103 => <<"12897979987">>,mti => <<"0200">>,
%%  bit => <<"B8000000000000000000000006000000">>}
%% 164> 
%%'''
%% @end
-spec unpack(IsoMessage,Specification) ->map() when
	IsoMessage		:: [integer()],
	Specification  	:: map().
unpack(IsoMessage,Specification)-> 
	iso8583_process:unpack(IsoMessage,Specification).


%% @doc this is for getting a particular field,mti,bitmap in an iso message back
%%
%% if getting a field,number  must be between 2 and 128.
%%
%% example below
%%```
%% 175> iso8583_erl:get_field(4,Map_message).
%% {ok,<<"0000000004.5">>}
%% 176>
%%'''
%% @end 
-spec get_field(Fld_num::pos_integer()|mti|bit,Iso_Map::map())->{ok,term()}|error.
get_field(Fld_num,Iso_Map)->
	iso8583_process:get_field(Fld_num,Iso_Map).


%%  @doc for setting the fields for an iso message
%%
%% field number must be between 2 and 128
%%
%% example below for setting the primary account number(PAN) i.e. field 2
%%```
%% 180> iso8583_erl:set_field(maps:new(),2,<<"4846801212341234129">>).
%% {ok,#{2 => <<"4846801212341234129">>}}
%% 181> 
%%'''
%% @end
-spec set_field(Iso_Map::map(),Fld_num::pos_integer() | mti ,Fld_val::binary())->{ok,map()}.
set_field(Iso_Map,Fld_num,Fld_val)->
	iso8583_process:set_field(Iso_Map,Fld_num,Fld_val).


%% @doc for setting multiple  fields at the same time
%% if setting a field,number must be between 2 and 128
%%
%% example below
%% ```
%% 183>  iso8583_erl:set_field_list([{mti,<<"0200">>},{3,<<"201234">>},{4,<<"4.5">>},{5,<<"5000">>},{102,<<"123413243">>},{103,<<"12897979987">>}]).
%% #{3 => <<"201234">>,4 => <<"4.5">>,5 => <<"5000">>,
%% 102 => <<"123413243">>,103 => <<"12897979987">>,
%% mti => <<"0200">>}
%% 184>  
%%'''
%% @end
-spec set_field_list(List::list())->map().
set_field_list(List)->
	iso8583_process:set_field_list(List).


%% @doc this is for setting the mti of a message
%% 
%% example below for setting the mti for a reversal(1420)
%%
%% ```
%% 185> iso8583_erl:set_mti(maps:new(),<<"1420">>).
%% {ok,#{mti => <<"1420">>}}
%% 186> 
%%'''
%% @end
-spec set_mti(Iso_Map::map(),Fld_val::binary())->{ok,map()}.
set_mti(Iso_Map,Mti_val)->
	iso8583_process:set_mti(Iso_Map,Mti_val).


%% @doc for creating the final bitmap
%%
%% this bitmap is an 8/16 byte binary with each byte being represented  by an integer.
%%
%% integer converted to a an 2 bit binary represents presence or absence of those fields
%%
%% example below
%%```
%% 8> iso8583_erl:create_bitmap(hex,
%% .. <<0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).
%% "3800000000000000"
%% 9>
%%'''
%% @end
-spec create_bitmap(binary|hex,binary())->binary()|list().
create_bitmap(Type_bitmap,Bitmap_final_bit)->
	iso8583_process:create_bitmap(Type_bitmap,Bitmap_final_bit).


%% @doc for getting the final size of the message to be sent
%%
%%this is usually done because the receiver of the message expects a header containing the size of the whole message so
%%
%%message can be processed on the receiving end of the socket without the socket being closed when a new message is being sent.
%%
%% the length of the header which will hold the size of the message is the second arguement.
%%
%% example below
%%```
%% 12> Message_send = iso8583_erl:set_field_list([{mti,<<"0200">>},{3,<<"201234">>},{4,<<"4.5">>},{5,<<"5000">>},{102,<<"123413243">>},{103,<<"12897979987">>}]).
%% #{3 => <<"201234">>,4 => <<"4.5">>,5 => <<"5000">>,102 => <<"123413243">>,103 => <<"12897979987">>,mti => <<"0200">>}
%% 13> Iso_Response = iso8583_erl:pack(Message_send,Spec).
%% ["0200","B8000000000000000000000006000000",["201234","0000000004.5","000000005000",["09","123413243"],["11","12897979987"]]]
%% 14> Final_size =  iso8583_erl:get_size_send(Iso_Response,4).
%% "0090"
%% Final_socket_send = [Final_size,Iso_Response],
%% io:format("~n sent server message is ~p",[Final_socket_send]),	 
%% case gen_tcp:connect(Host, Port, [list, {active, once}]) of
%%      {ok, Socket} ->
%%			ok = gen_tcp:send(Socket,Final_socket_send),
%%			ok = gen_tcp:close(Socket);
%%		{error, Error} ->
%%            error_logger:format("Connection failed: ~ts~n", [inet:format_error(Error)])
%% end.		 
%% 15> 
%%'''
%% @end
-spec get_size_send(iolist(),non_neg_integer())->list(). 
get_size_send(Fields_iolist,Length_max_size)->
	iso8583_process:get_size_send(Fields_iolist,Length_max_size).
%%====================================================================
%% Internal functions
%%====================================================================
