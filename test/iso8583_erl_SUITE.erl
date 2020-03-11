-module(iso8583_erl_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([init_per_suite/1,end_per_suite/1,init_per_testcase/2,end_per_testcase/2,all/0]).
-export([pack_data/1,set_field/1,unpack_data/1,unpack_data_secodary_bitmap_message/1,unpack_data_primary_bitmap_message/1,process_data_element/1,create_bitmap_binary/1,create_bitmap_hex/1,create_bitmap_spec/1,get_bitmap_subs/1,pack_data_2/1]).



all() -> [pack_data,set_field,unpack_data,unpack_data_secodary_bitmap_message,unpack_data_primary_bitmap_message,process_data_element,create_bitmap_binary,create_bitmap_hex,create_bitmap_spec,get_bitmap_subs,pack_data_2].

init_per_suite(Config) ->
    ok = application:ensure_started(iso8583_erl),
    Config.

end_per_suite(_Config) ->
    application:stop(iso8583_erl).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.


set_field(_Config)->
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200),
	{ok,Second_map} = iso8583_erl:set_field(First_map,3,201234),
	{ok,Third_map} = iso8583_erl:set_field(Second_map,4,123456789012),
	{ok,Fourth_map} = iso8583_erl:set_field(Third_map,5,5000),
	?assertEqual(true,#{3 => 201234,4 => 123456789012,5 => 5000,mti => 200} =:= Fourth_map).
	
pack_data(_Config) ->
	%%one way to pack data
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200),
	{ok,Second_map} = iso8583_erl:set_field(First_map,3,201234),
	{ok,Third_map} = iso8583_erl:set_field(Second_map,4,4.5),
	{ok,Fourth_map} = iso8583_erl:set_field(Third_map,5,5000),
	{ok,Fifth_map} = iso8583_erl:set_field(Fourth_map,102,"123413243"),
	{ok,Six_map} = iso8583_erl:set_field(Fifth_map,103,"12897979987"),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Six_map,Specification),
	%another way to pack data
	Map_send_list = iso8583_erl:set_field_list([{mti,0200},{3,201234},{4,4.5},{5,5000},{102,"123413243"},{103,"12897979987"}]),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Map_send_list,Specification).


pack_data_2(_Config)->
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	Map_send_list = iso8583_erl:set_field_list([{mti,0200},{2,1231231312},{4,1000.5},{7,1107221800},{11,1},{12,200217},{18,1234},{19,233},
	{20,233},{39,"00"},{40,100},{41,"EBS00001"},{101,"test_file.civ"},{102,"1111111111"},{103,"00101010101001"}]),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Map_send_list,Specification),
	Final_1 =  lists:append([Mti,Bitmap_final_bit,lists:append(Fields_list)]),
	?assertEqual(true,Final_1 =:= "0200D230700003800000000000000E0000001012312313120000001000.51107221800000001200217123423323300100EBS0000113test_file.civ1011111111111400101010101001").

unpack_data(_Config)->
	%%pack the data first
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200),
	{ok,Second_map} = iso8583_erl:set_field(First_map,3,201234),
	{ok,Third_map} = iso8583_erl:set_field(Second_map,4,123456789012),
	{ok,Fourth_map} = iso8583_erl:set_field(Third_map,5,123456789012),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Fourth_map,Specification),
	Final_fields = [Mti,Bitmap_final_bit,lists:append(Fields_list)],
	Map_data = #{3 => 201234,4 => 123456789012,5 => 123456789012,
    bit => <<"3800000000000000">>,mti => <<"0200">>},
	?assertEqual(true,Map_data =:= iso8583_erl:unpack(Final_fields,Specification)).


unpack_data_secodary_bitmap_message(_Config)->
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	Result = 
	 #{2 => 1231231312,4 => 1000.5,7 => 1107221800,11 => 1,
	  12 => 200217,18 => 1234,19 => 233,20 => 233,39 => "00",
	  40 => 100,41 => "EBS00001",101 => "test_file.civ",
	  102 => "1111111111",103 => "00101010101001",
	  bit => <<"D230700003800000000000000E000000">>,mti => <<"0200">>},	  
	Result = iso8583_erl:unpack(
	"0200D230700003800000000000000E0000001012312313120000001000.51107221800000001200217123423323300100EBS0000113test_file.civ1011111111111400101010101001",Specification).

unpack_data_primary_bitmap_message(_Config)->
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	Result = 
	  #{2 => 1231231312,4 => 1000.5,7 => 1107221800,11 => 1,
	  12 => 200217,18 => 1234,19 => 233,20 => 233,39 => "00",
	  40 => 100,41 => "EBS00001",
	  bit => <<"5230700003800000">>,mti => <<"0200">>},
	Result = iso8583_erl:unpack("020052307000038000001012312313120000001000.51107221800000001200217123423323300100EBS00001",Specification).


process_data_element(_Config)->
	%%for processing an item given an binary showing presence or absence of fields
	%%,the index to start from from the spec file in a sequential manner  and the specification file
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	Reponse = iso8583_erl:process_data_element(<<"011100000000000000000000000000000000000000000000000000000000000">>,2,<<"201234123456789012123456789012">>,Specification),
	?assertEqual(true,#{3 => 201234,4 => 123456789012,5 => 123456789012} =:= Reponse).
	


create_bitmap_binary(_Config)->
	%%for creating a  binary bitmap based on the a  binary containing 0/1 showing presence or absence of fields
	<<56,0,0,0,0,0,0,0>> = iso8583_erl:create_bitmap(binary,
	<<0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).


create_bitmap_hex(_Config)->
	%%for creating a  hex bitmap based on the a  binary containing 0/1 showing presence or absence of fields
	"3800000000000000" = iso8583_erl:create_bitmap(hex,
	<<0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).


create_bitmap_spec(_Config)->
	%%for creating a  hex bitmap based on module specification
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	"3800000000000000" = iso8583_erl:create_bitmap(iso8583_erl:get_bitmap_type(Specification),
	<<0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).

	
get_bitmap_subs(_Config)->
	%%get the mti,the text binary and the integer binary,as well as the text fields converted to a binary
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	{Mti,Text_Binary,Integer_binary,Binary_fields} = {<<"0200">>,<<"0011100000000000000000000000000000000000000000000000000000000000">>,
	<<56,0,0,0,0,0,0,0>>,<<"201234123456789012123456789012">>},
	Response = 
	iso8583_erl:get_bitmap_subs(binary,<<48,50,48,48,56,0,0,0,0,0,0,0,50,48,49,50,51,52,49,50,51,52,53,54,55,56,57,48,49,50,49,50,51,52,53,54,55,56,57,48,49,50>>,Specification),
	{Mti,Text_Binary,Integer_binary,Binary_fields} = Response.
