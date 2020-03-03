iso8583_erl
=====

An erlang library for processing iso8583 messages inspired by [erl8583],[jpos library].

This is a minimalist library for  packing and unpacking iso8583 financial messages.

It accepts a specification file which contains information about fields,mti,bitmap  for your interchange.

It then uses those specifications to then pack and unpack iso messages for your interchange.





## quickstart ##
```erlang

%set mti
Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,Specification),


%set field
{ok,Second_map} = iso8583_erl:set_field(First_map,3,201234,Specification),


%pack_data
{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,Specification),
{ok,Second_map} = iso8583_erl:set_field(First_map,3,201234,Specification),
{ok,Third_map} = iso8583_erl:set_field(Second_map,4,4.5,Specification),
{ok,Fourth_map} = iso8583_erl:set_field(Third_map,5,5000,Specification),
[Mti,Bitmap_final_bit,Fields_list]  = iso8583_erl:pack(Fourth_map,Specification),




%another way to pack data
{ok,First_map} = iso8583_erl:set_mti(maps:new(),0200,Specification),	
Iso_vals = [{3,201234},{4,4.5},{5,5000}],
Map_send_list = lists:foldl(
	fun({Key,Value},Acc)->
		{ok,Map_new_Accum} = iso8583_erl:set_field(Acc,Key,Value,Specification),
		Map_new_Accum
	end,First_map,Iso_vals),
[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Map_send_list,Specification).


%%third way to pack data
Iso_vals_new = [{mti,0200},{3,201234},{4,4.5},{5,5000}],
Map_send_list = iso8583_erl:set_field_list(Iso_vals_new,Specification),




%%send to receiving interface server after packing 
%%should have been packed first to get  the following list [Mti,Bitmap_final_bit,Fields_list]
Final_length = iso8583_ascii:get_size_send(Mti,Bitmap_final_bit,Fields_list),
%% zero padded to a 2 byte header
Send_list_final = [<<0,Final_length>>,Mti,Bitmap_final_bit,Fields_list],
%%send to interface
ok = gen_tcp:send(Socket,Send_list_final),
ok = inet:setopts(Socket, [{active, once}]).


%unpack data
Message = "02003800000000000000201234123456789012123456789012",
Result = iso8583_erl:unpack(Message,Specification).
io:format("Result is ~p",[Result]).
Result is #{3 => 201234,4 => 123456789012,5 => 123456789012,
		<<"bit">> => <<"3800000000000000">>,<<"mti">> => <<"0200">>}
```


more examples of usage can be found in  ```test/iso8583_erl_SUITE.erl```


## acknowledgements ##
i wish to thank the creators of the jpos library from which gave a lot of great ideas.

i also wish to thank the creators of erl8583 library which gave a lot of excellent ideas.

[erl8583]: https://github.com/mgwidmann/erl8583
[jpos library]: https://github.com/jpos/jPOS
