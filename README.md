iso8583_erl
=====

An erlang library for processing iso8583 messages inspired by [erl8583],[jpos library].

This is a minimalist library for  packing and unpacking iso8583 financial messages.

It accepts a module which contains specification information about fields,mti,bitmap  for your interchange.

It then uses those specifications to then pack and unpack iso messages for your interchange.





## quickstart ##
```erlang

%set mti
{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,iso8583_ascii_def),


%set field
{ok,First_map} = iso8583_erl:set_field(maps:new(),3,201234,iso8583_ascii_def),


%pack_data
{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,iso8583_ascii_def),
{ok,Second_map} = iso8583_erl:set_field(First_map,3,201234,iso8583_ascii_def),
{ok,Third_map} = iso8583_erl:set_field(Second_map,4,4.5,iso8583_ascii_def),
{ok,Fourth_map} = iso8583_erl:set_field(Third_map,5,5000,iso8583_ascii_def),
[Mti,Bitmap_final_bit,Fields_list]  = iso8583_erl:pack(Fourth_map,iso8583_ascii_def),




%another way to pack data
{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,iso8583_ascii_def),	
Iso_vals = [{3,201234},{4,4.5},{5,5000}],
Map_send_list = lists:foldl(
	fun({Key,Value},Acc)->
		{ok,Map_new_Accum} = iso8583_erl:set_field(Acc,Key,Value,iso8583_ascii_def),
		Map_new_Accum
	end,First_map,Iso_vals),
[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Map_send_list,iso8583_ascii_def).


%%send to receiving interface server after packing 
%%should have been packed first to get  the following list [Mti,Bitmap_final_bit,Fields_list]
Size_Bitmap = iso8583_ascii:get_size(bitmap,Bitmap_final_bit),
Final_size = iso8583_ascii:get_size(field_list,Fields_list),
Final_length = length(Mti)+Size_Bitmap+Final_size,
%% zero padded to a 2 byte header
Final_size_pad = string:right(erlang:integer_to_list(Final_length),?2,$0),
Send_list_final = [<<0,Final_length>>,Mti,Bitmap_final_bit,Fields_list],
%%send to interface
ok = gen_tcp:send(Socket,Send_list_final),
ok = inet:setopts(Socket, [{active, once}]).


%unpack data
Message = "02003800000000000000201234123456789012123456789012",
Result = iso8583_erl:unpack(Message,iso8583_ascii_def).
io:format("Result is ~p",[Result]).
Result is #{3 => 201234,4 => 123456789012,5 => 123456789012,
		<<"bit">> => <<"3800000000000000">>,<<"mti">> => <<"0200">>}
```





## exports ##

iso8583_erl exports the following functions

```erlang

set_mti/3
for setting the mti


set_field/3
for setting the field


unpack/2
for unpacking iso8583 messages


pack/2
for packing iso8583 messages


get_size/2
for getting the size of an iso8583 message list or bitmap


set_field/4
for setting the field


set_mti/3
for setting the mti


process_data_element/4
for processing an item given an binary showing presence or absence of fields


create_bitmap/2
for creating the bitmap


get_bitmap_subs/3
for getting the mti,bitmap,iso message and other elements from an iso message


```


## acknowledgements ##
i wish to thank the creators of the jpos library from which gave a lot of great ideas.

i also wish to thank the creators of erl8583 library which gave a lot of excellent ideas.

[erl8583]: https://github.com/mgwidmann/erl8583
[jpos library]: https://github.com/jpos/jPOS
