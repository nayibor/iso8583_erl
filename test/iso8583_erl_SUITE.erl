-module(iso8583_erl_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([init_per_suite/1,end_per_suite/1,init_per_testcase/2,end_per_testcase/2,all/0]).
-export([pack_data/1,set_field/1,unpack_data/1,set_field/1,process_data_element/1,create_bitmap_binary/1,create_bitmap_hex/1,create_bitmap_spec/1,get_bitmap_subs/1]).

%%common test definitions for the usermod functions
%%testing whether istill dey happen

all() -> [pack_data,set_field,unpack_data,set_field,process_data_element,create_bitmap_binary,create_bitmap_hex,create_bitmap_spec,get_bitmap_subs].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:start(iso8583_erl),
    Config.

end_per_suite(_Config) ->
    application:stop(iso8583_erl).

init_per_testcase(_, Config) ->
	io:format("starting iso8583_erl tests"),
    Config.

	
end_per_testcase(_, _Config) ->
    ok.


set_field(_Config)->
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,iso8583_ascii_def),
	{ok,Second_map} = 	iso8583_erl:set_field(First_map,3,201234,iso8583_ascii_def),
	{ok,Third_map} = 	iso8583_erl:set_field(Second_map,4,123456789012,iso8583_ascii_def),
	{ok,Fourth_map} = 	iso8583_erl:set_field(Third_map,5,5000,iso8583_ascii_def),
	?assertEqual(true,#{3 => "201234",4 => "123456789012",5 => "000000005000",mti => "0200"} =:= Fourth_map).
	
pack_data(_Config) ->
	%%one way to pack data
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,iso8583_ascii_def),
	{ok,Second_map} = 	iso8583_erl:set_field(First_map,3,201234,iso8583_ascii_def),
	{ok,Third_map} = 	iso8583_erl:set_field(Second_map,4,4.5,iso8583_ascii_def),
	{ok,Fourth_map} = 	iso8583_erl:set_field(Third_map,5,5000,iso8583_ascii_def),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Fourth_map,iso8583_ascii_def),
	%another way to pack data
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,iso8583_ascii_def),	
	Iso_vals = [{3,201234},{4,4.5},{5,5000}],
	Map_send_list = lists:foldl(
		fun({Key,Value},Acc)->
			{ok,Map_new_Accum} = iso8583_erl:set_field(Acc,Key,Value,iso8583_ascii_def),
			Map_new_Accum
		end,First_map,Iso_vals),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Map_send_list,iso8583_ascii_def).


unpack_data(_Config)->
	%%pack the data first
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,iso8583_ascii_def),
	{ok,Second_map} = 	iso8583_erl:set_field(First_map,3,201234,iso8583_ascii_def),
	{ok,Third_map} = 	iso8583_erl:set_field(Second_map,4,123456789012,iso8583_ascii_def),
	{ok,Fourth_map} = 	iso8583_erl:set_field(Third_map,5,123456789012,iso8583_ascii_def),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Fourth_map,iso8583_ascii_def),
	Final_fields = [Mti,Bitmap_final_bit,lists:append(Fields_list)],
	ct:pal("message is ~p",[Final_fields]),
	Result = iso8583_erl:unpack(Final_fields,iso8583_ascii_def).


process_data_element(_Config)->
	%%for processing an item given an binary showing presence or absence of fields
	%%,the index to start from from the spec file in a sequential manner  and the specification file
	Reponse = iso8583_erl:process_data_element(<<"011100000000000000000000000000000000000000000000000000000000000">>,2,<<"201234123456789012123456789012">>,iso8583_ascii_def),
	?assertEqual(true,#{3 => 201234,4 => 123456789012,5 => 123456789012} =:= Reponse).
	


create_bitmap_binary(_Config)->
	%%for creating a  binary bitmap based on the a  binary containing 0/1 showing presence or absence of fields
	Module_process = iso8583_ascii_def,
	<<56,0,0,0,0,0,0,0>> = iso8583_erl:create_bitmap(binary,
	<<0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).


create_bitmap_hex(_Config)->
	%%for creating a  hex bitmap based on the a  binary containing 0/1 showing presence or absence of fields
	Module_process = iso8583_ascii_def,
	"3800000000000000" = iso8583_erl:create_bitmap(hex,
	<<0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).


create_bitmap_spec(_Config)->
	%%for creating a  bitmap based on specification
	Module_process = iso8583_ascii_def,
	"3800000000000000" = iso8583_erl:create_bitmap(Module_process:get_bitmap_type(),
	<<0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).

	
get_bitmap_subs(_Config)->
	%%get the mti,the text binary and the integer binary,as well as the text fields converted to a binary
	{Mti,Text_Binary,Integer_binary,Binary_fields} = {<<"0200">>,<<"0011100000000000000000000000000000000000000000000000000000000000">>,
	<<56,0,0,0,0,0,0,0>>,<<"201234123456789012123456789012">>},
	Response = 
	iso8583_erl:get_bitmap_subs(binary,<<48,50,48,48,56,0,0,0,0,0,0,0,50,48,49,50,51,52,49,50,51,52,53,54,55,56,57,48,49,50,49,50,51,52,53,54,55,56,57,48,49,50>>,iso8583_ascii_def),
	{Mti,Text_Binary,Integer_binary,Binary_fields} = Response.
