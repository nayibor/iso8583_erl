iso8583_erl
=====

An erlang library for processing iso8583 messages inspired by [erl8583],[jpos library].

This is a minimalist library for  packing and unpacking iso8583 financial messages.

It accepts a specification file which contains information about fields,mti,bitmap  for your interchange.

It then uses those specifications to then pack and unpack iso messages for your interchange.

The specification can be loaded once and then used for all packing and unpacking operations


## usage ##

the library can be used by putting the following in your rebar.config as a dep
```erlang
{iso8583_erl,{git,"https://github.com/nayibor/iso8583_erl.git",{tag,"1.0.0"}}}
```


## quickstart ##
```erlang

%set mti
	Specification = iso8583_erl:load_specification(code:priv_dir(iso8583_erl)++"/custom.cfg"),
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),<<"0200">>),


%set field
	{ok,Second_map} = iso8583_erl:set_field(First_map,3,<<"201234">>),


%pack_data
	%%one way to pack data
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),<<"0200">>),
	{ok,Second_map} = iso8583_erl:set_field(First_map,3,<<"201234">>),
	{ok,Third_map} = iso8583_erl:set_field(Second_map,4,<<"4.5">>),
	{ok,Fourth_map} = iso8583_erl:set_field(Third_map,5,<<"5000">>),
	{ok,Fifth_map} = iso8583_erl:set_field(Fourth_map,102,<<"123413243">>),
	{ok,Six_map} = iso8583_erl:set_field(Fifth_map,103,<<"12897979987">>),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Six_map,Specification),
	%another way to pack data
	Map_send_list = iso8583_erl:set_field_list([{mti,<<"0200">>},{3,<<"201234">>},{4,<<"4.5">>},{5,<<"5000">>},{102,<<"123413243">>},{103,<<"12897979987">>}]),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Map_send_list,Specification).


%%send to receiving interface server after packing 
	%%should have been packed first to get  the following list [Mti,Bitmap_final_bit,Fields_list]
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),<<"0200">>),
	{ok,Second_map} = iso8583_erl:set_field(First_map,3,<<"201234">>),
	{ok,Third_map} = iso8583_erl:set_field(Second_map,4,<<"4.5">>),
	{ok,Fourth_map} = iso8583_erl:set_field(Third_map,5,<<"5000">>),
	{ok,Fifth_map} = iso8583_erl:set_field(Fourth_map,102,<<"123413243">>),
	{ok,Six_map} = iso8583_erl:set_field(Fifth_map,103,<<"12897979987">>),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Six_map,Specification),
	Final_length = iso8583_erl:get_size_send(Mti,Bitmap_final_bit,Fields_list),
	%% zero padded to a 2 byte header
	Send_list_final = [<<0,Final_length>>,Mti,Bitmap_final_bit,Fields_list],
	%%send to interface
	ok = gen_tcp:send(Socket,Send_list_final),
	ok = inet:setopts(Socket, [{active, once}]).


%unpack data
	{ok,First_map} = iso8583_erl:set_mti(maps:new(),<<"0200">>),
	{ok,Second_map} = iso8583_erl:set_field(First_map,3,<<"001234">>),
	{ok,Third_map} = iso8583_erl:set_field(Second_map,4,<<"123456789012">>),
	{ok,Fourth_map} = iso8583_erl:set_field(Third_map,5,<<"123456789012">>),
	[Mti,Bitmap_final_bit,Fields_list] = iso8583_erl:pack(Fourth_map,Specification),
	Final_fields = [Mti,Bitmap_final_bit,Fields_list],
	Map_data = #{3 => <<"001234">>,4 => <<"123456789012">>,5 => <<"123456789012">>,
    bit => <<"3800000000000000">>,mti => <<"0200">>},
	?assertEqual(true,Map_data =:= iso8583_erl:unpack(Final_fields,Specification)).

```


more examples of usage can be found in  ```test/iso8583_erl_SUITE.erl```


## acknowledgements ##
i wish to thank the creators of the jpos library from which gave a lot of great ideas.

i also wish to thank the creators of erl8583 library which gave a lot of excellent ideas.

[erl8583]: https://github.com/mgwidmann/erl8583
[jpos library]: https://github.com/jpos/jPOS
