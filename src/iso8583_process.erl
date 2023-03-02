%%%
%%% @doc iso8583_ascii module.
%%%<br>this module is responsible for processing string iso messages  format</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_process).

-export([unpack/2,pack/2,set_field/3,set_field_list/1,set_mti/2,get_field/2,process_data_element/4,create_bitmap/2,
		get_bitmap_subs/3,get_size_send/2,load_specification/1,get_spec_field/2,get_bitmap_type/1,
		convert_base_pad/3,load_specification_mti/1,get_spec_mti/3,check_mandatory_fields/2,
		add_echo_fields/3
		]).


%% @doc this is for performing a binary fold kind of like a list fold
-spec fold_bin(Fun, T, Bin) -> T when
	Fun  		:: fun((Input, T) -> {Reminder, T}),
	Bin  		:: binary(),
	Input		:: binary(),
	Reminder	:: binary().
%% base case first
fold_bin(_Fun, Accum, <<>>) -> Accum;
fold_bin(Fun, Accum, Bin) ->
		{NewBin, NewAccum} = Fun(Bin, Accum),
		fold_bin(Fun, NewAccum, NewBin).

	  	
%% @doc this converts data between bases and also pads the data  for our purposes
-spec convert_base_pad(integer(),integer(),binary())->binary().
convert_base_pad(Data_Base_10,Number_pad,Pad_digit)->
        Data_base2 = erlang:integer_to_binary(Data_Base_10,2),
        pad_data_new(Data_base2,Number_pad,Pad_digit,right).


%% @doc creats a new map specification which contains the various data elements and a bitmap from a specification file
-spec load_specification(string() |binary())->map().
load_specification(Filename)->
		{ok,Spec_data} = file:consult(Filename),
		 Map_spec = maps:new(),
		 lists:foldl(
		 fun({Key,Value},Acc)->
			case Key of 
				bitmap_type->
					maps:put(bitmap_type,Value,Acc);
				Number when Number >=1,Number =<128,is_integer(Number) ->
					#{pad_info := Pad_info,header_length := Header_length,length_field := Length_field,sub_format:=Format} = Value,
					Fl_vl = fixed_variable(Header_length),
					maps:put(Number,{Length_field,Fl_vl,Header_length,Format,Pad_info},Acc);
				_ ->
					Acc
			end
		end,Map_spec,Spec_data).


%% @doc this is for loading field information for mtis
-spec load_specification_mti(string() |binary())->map().
load_specification_mti(Filename)->
		{ok,Spec_data} = file:consult(Filename),
		Map_spec = maps:new(),
		lists:foldl(
		 fun({Key,Value},Acc)->
			case Key of 
				Key_value when is_binary(Key_value),size(Key_value)=:=4 ->
					Key_list = maps:keys(Value),	
					Result =  lists:map(fun(Key_mti)->lists:member(Key_mti,Key_list) end,[mand,opti,echo,condi,snno,snrr]),
					case  lists:member(false,Result) of
						true ->
							throw(error);
						 false ->
								maps:put(Key_value,Value,Acc)
					end;	
				_ ->
					Acc
			end
		end,Map_spec,Spec_data).		 


%% @doc for checking mandatory fields
-spec check_mandatory_fields(list(),map())->true|false.
check_mandatory_fields(List_mandatory_keys,Iso_map)->
	Map_fields = maps:keys(Iso_map),
	Result =  lists:map(fun(Field_mand)->lists:member(Field_mand,Map_fields) end,List_mandatory_keys),
	case lists:member(false,Result) of
		true ->
			false;
		false ->
			true
	end.
		

%% @doc for adding fields which are supposed to be echoed
-spec add_echo_fields(map(),map(),map())->map().
add_echo_fields(Map_transaction,Map_recipient,Specification_mti)->
	{ok,Mti}  = get_field(mti,Map_recipient),
	List_echo_fields = get_spec_mti(echo,Mti,Specification_mti),
	lists:foldl(
	fun(Field,Acc)->
		case maps:get(Field,Map_transaction,error) of 
			error ->
				Acc;
			Field_value ->
				maps:put(Field,Field_value, Acc)
		end
	end,Map_recipient,List_echo_fields).
	
	

%% @doc for getting various specification types
-spec get_spec_mti(atom(),binary(),map())->list()|error.
get_spec_mti(Spec_type,Mti,Spec_field_map)->		
	Spec_mti = maps:get(Mti,Spec_field_map),
	maps:get(Spec_type,Spec_mti).


%% @doc finds out if a field is fixed length or variable lengt
-spec fixed_variable(non_neg_integer())->fx|vl.
fixed_variable(Number) when Number =:= 0,is_integer(Number)->fx;
fixed_variable(Number) when Number > 0,is_integer(Number)->vl.


%% @doc gets the specification for a particular field
-spec get_spec_field(non_neg_integer()|mti,map())->tuple().
get_spec_field(Field,Specification)->
	maps:get(Field,Specification).


%% @doc gets the bitmap type
-spec get_bitmap_type(map())->atom().
get_bitmap_type(Specification)->
	maps:get(bitmap_type,Specification).


%% @doc this part accepts a list iso message with the header removed and extracts the mti,bitmap,data elements into a map object 
%% it also accepts a specification which will be used for getting the specifications for the message
%% exceptions can be thrown here if the string for the message hasnt been formatted well but they should be caught in whichever code is calling the system 
%%the data is first converted into a binary before the processing is done . much faster and uses less memory than using lists
-spec unpack(list(),map())->map().
unpack(Rest,Specification)-> 
		Bin_message = erlang:list_to_binary(Rest),
		process_binary(Bin_message,Specification).


%% @doc this function is used to derive various fields given an iso message and the message area(iso 1987,1992,2002,postillion,ascii subfield etc .. works with ascii )
%% can be used for getting various iso message fields as well as getting subfields out of an iso message
%%all data needed to calculate bitmamp should be part of input to this function 
-spec process_binary(binary(),map())->map().
process_binary(Bin_message,Specification)->
		Bitmap_type = get_bitmap_type(Specification),
		{Mti,Bit_mess,Bitmap_Segment,Rest} = get_bitmap_subs(Bitmap_type,Bin_message,Specification),
		<<_:1/binary,Real_bitmap/binary>> = Bit_mess,
		Mti_map = maps:put(mti,Mti,maps:new()),
		Map_Init = maps:put(bit,Bitmap_Segment,Mti_map),
		Result_process = process_data_element(Real_bitmap,2,Rest,Specification),
		maps:merge(Result_process,Map_Init).


%% @doc for processing the message given the bitmap and the binary containing the data elements
-spec process_data_element(binary(),integer(),binary(),map())->map().
process_data_element(Bitmap,Index_start,Data_binary,Specification)->
		Map_Init = maps:new(),
		OutData = fold_bin(
			 fun(<<X:1/binary, Rest_bin/binary>>, {Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= <<"1">> ->
					{Flength,Fx_var_fixed,Fx_header_length,_,_} = get_spec_field(Current_index_in,Specification),
					Data_index = case Fx_var_fixed of
						fx -> 
							Data_element_fx_raw = binary:part(Data_for_use_in,Index_start_in,Flength),
							New_Index_vl = Index_start_in+Flength,
							{Data_element_fx_raw,New_Index_vl};
						vl ->
							Header = binary:part(Data_for_use_in,Index_start_in,Fx_header_length),
							Header_value = erlang:binary_to_integer(Header),
							Start_val = Index_start_in + Fx_header_length,
							Data_element_vl_raw = binary:part(Data_for_use_in,Start_val,Header_value),
							New_Index_vl = Start_val+Header_value,
							{Data_element_vl_raw,New_Index_vl}
								end, 
					{Data_element,New_Index} = Data_index,
					NewMap = maps:put(Current_index_in,Data_element,Map_out_list_in),
					Fld_num_out = Current_index_in + 1, 
					{Rest_bin,{Data_for_use_in,New_Index,Fld_num_out,NewMap}};
			    (<<X:1/binary, Rest_bin/binary>>, {Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= <<"0">> ->
					Fld_num_out = Current_index_in + 1,					
					{Rest_bin,{Data_for_use_in,Index_start_in,Fld_num_out,Map_out_list_in}}
			end, {Data_binary,0,Index_start,Map_Init},Bitmap),
		{_,_,_,Fldata} = OutData,
		Fldata.


%% @doc marshalls a message to be sent.
%%pack all the differnt elements in a message into an iolist
-spec pack(Message_Map::map(),map())->iolist().
pack(Message_Map,Specification)->
		Pred = fun(Key,_) -> erlang:is_integer(Key) andalso (Key >= 65)  andalso (Key =< 128)  end,
		Secondary_bitmap_flag = maps:filter(Pred,Message_Map),
		case erlang:map_size(Secondary_bitmap_flag) of
			0->
				pack_message(primary,Message_Map,Specification);
			_ ->
				pack_message(secondary,Message_Map,Specification)
		end.


%% @doc creates a primary/secondary bitmap out of message map and specification
-spec pack_message(primary|secondary,map(),map())->iolist().
pack_message(primary,Message_Map,Specification)-> 
		{Bitmap_final,Iso_Fields_Binary} = lists:foldl((pack_check_keys(Message_Map,Specification)) ,{<<>>,[]},lists:seq(2,64)),
		Bitmap_final_bit = << 0,Bitmap_final/binary>>,
		Bitmap_final_bit_list = create_bitmap(get_bitmap_type(Specification),Bitmap_final_bit),
		{ok,Mti} = format_data(1,maps:get(mti,Message_Map),Specification),
		Fields_list = lists:reverse(Iso_Fields_Binary),
		[Mti,Bitmap_final_bit_list,Fields_list];


pack_message(secondary,Message_Map,Specification)-> 
		{Bitmap_final,Iso_Fields_Binary} = lists:foldl((pack_check_keys(Message_Map,Specification)) ,{<<>>,[]},lists:seq(2,128)),
		Bitmap_final_bit = << 1,Bitmap_final/binary>>,
		Bitmap_final_bit_list = create_bitmap(get_bitmap_type(Specification),Bitmap_final_bit),
		{ok,Mti} = format_data(1,maps:get(mti,Message_Map),Specification),
		Fields_list = lists:reverse(Iso_Fields_Binary),
		[Mti,Bitmap_final_bit_list,Fields_list].


%% @doc used for setting the bitmap fields for each field based on whether the key exists or not
%%returns anonymous  function which is used for setting up the bitmap
-spec pack_check_keys(maps:iterator()|map(),maps:iterator()|map())->fun().
pack_check_keys(Message_Map,Specification)->
		fun(Field_key,{Bitmap,Iso_Fields})->
			case maps:get(Field_key,Message_Map,error) of
				error ->
					New_Bitmap = << Bitmap/binary,0 >>,
					{New_Bitmap,Iso_Fields};
				Value ->
					New_Bitmap = << Bitmap/binary,1>>,
					{ok,Actual_value} = format_data(Field_key,Value,Specification),
					New_Iso_Fields = [Actual_value|Iso_Fields],
					{New_Bitmap,New_Iso_Fields}
			end
		end.


%% @doc for getting the bitmap,mti,Data fields 
-spec get_bitmap_subs(atom(),binary(),map())-> tuple().
get_bitmap_subs(binary,Bin_message,Specification)->
		{Flength,_,_,_,_} = get_spec_field(1,Specification),
		<<One_dig/integer>> = binary_part(Bin_message,Flength,1),
		Bitsize = 
		case  binary_part(convert_base_pad(One_dig,8,<<"0">>),0,1) of
			<<"0">> -> 8;
			<<"1">> -> 16
	    end,		
		<<Mti:Flength/binary,Bitmap_Segment:Bitsize/binary,Rest/binary>> = Bin_message,
		Bit_mess = << << (convert_base_pad(One,8,<<"0">>))/binary >>  || <<One>> <= Bitmap_Segment >>,
		{Mti,Bit_mess,Bitmap_Segment,Rest};


get_bitmap_subs(hex,Bin_message,Specification)->
		{Flength,_,_,_,_} = get_spec_field(1,Specification),
		One_dig = binary_part(Bin_message,Flength,1),
		Size_base_ten = erlang:binary_to_integer(One_dig,16),
		Bitsize = 
		case  Size_base_ten =< 7 of
			true -> 16;
			false -> 32
		end,
		<<Mti:Flength/binary,Bitmap_Segment:Bitsize/binary,Rest/binary>> = Bin_message,
		Bit_mess = fold_bin(
			 fun(<<X:2/binary, Rest_bin/binary>>,Bin_list_final) ->
				 Base_10  = erlang:binary_to_integer(X,16),
				 Converted_base  = << (convert_base_pad(Base_10,8,<<"0">>))/binary >>,
			    List_oct =  << Bin_list_final/binary,Converted_base/binary  >>,
			    {Rest_bin,List_oct}
		     end,<<>>,Bitmap_Segment),
		{Mti,Bit_mess,Bitmap_Segment,Rest}.



%% @doc for creating the final bitmap
%%this bitmap is an 8/16 byte binary with each byte being represented  by an integer.
%%integer converted to a an 2 bit binary represents presence or absence of those fields
-spec create_bitmap(binary|hex,binary())->binary()|list().
create_bitmap(binary,Bitmap_final_bit)->
		fold_bin(
			 fun(<<X:8/binary, Rest_bin/binary>>,Bin_list_final) ->
				List_bin = erlang:binary_to_list(X),
				List_string = lists:foldr(fun(X_fold,Acc)-> C = erlang:integer_to_list(X_fold),[C|Acc]end,[],List_bin),
				Lists_string_app = lists:append(List_string),
				Bitmap_oct = erlang:list_to_integer(Lists_string_app,2),
			    List_oct =  << Bin_list_final/binary,Bitmap_oct/integer  >>,
			    {Rest_bin,List_oct}
		     end,<<>>,Bitmap_final_bit);


%%this is for creating a hexadecimal bitmap  
create_bitmap(hex,Bitmap_final_bit)->
		Bitmap_hex = 
		fold_bin(
			fun(<<X:8/binary, Rest_bin/binary>>,Accum_list) ->
				First_conv = erlang:binary_to_list(X),
				Concat_First_conv  = lists:concat(First_conv),
				Concat_First_conv_base = erlang:list_to_integer(Concat_First_conv,2),				
				List_part = string:right(erlang:integer_to_list(Concat_First_conv_base,16),2,$0),
				{Rest_bin, [List_part | Accum_list]}
			end,[],Bitmap_final_bit),
		lists:append(lists:reverse(Bitmap_hex)).



%% @doc this will be used for formatting the data which is sent 
%%it is done at the setting stage
%%it checks if data is of the correct length and type for numbers and simple strings and binaries
%%%also adds paddings and as well as headers to the various values
%%not full featured but just enough to make it work
-spec format_data(integer()|mti,term(),map())->{ok,term()}.
format_data(Key,Value,Specification)->
		{Flength,Fx_var_fixed,Fx_header_length,Sub_format,{Pad_info,Pad_char}}  = get_spec_field(Key,Specification),
		pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Value,Pad_char,Sub_format,Pad_info).



%% @doc for changing input binary to string based on the data type 
-spec change_binary_string(binary(),term())->binary()|string().
change_binary_string(Input_binary,Data_type)->
	case Data_type of
		"B"->
			Input_binary;
		_ ->
			erlang:binary_to_list(Input_binary)
	end.

%%for padding various fields based on whether its a variable length field or a fixed length field
-spec pad_data_check(fx|vl,integer(),integer(),binary()|list(),char()|atom()|binary(),term(),atom())->{ok,list()}|{ok,binary()}.
pad_data_check(Fx_var_fixed,Fx_header_length,_Flength,Numb_check,_Pad_char,Sub_format,none)->
		case Fx_var_fixed of 
			fx->
				{ok,change_binary_string(Numb_check,Sub_format)};
			vl->
				Size =  erlang:size(Numb_check),
				Fsize = string:right(erlang:integer_to_list(Size),Fx_header_length,$0),							
				Final_list = [Fsize,change_binary_string(Numb_check,Sub_format)],
				{ok,Final_list}
		end;


pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,Char_pad,Sub_format,Pad_direction)->
		case Fx_var_fixed of 
			fx->
				Padded_data = pad_data_new(Numb_check,Flength,Char_pad,Pad_direction),
				{ok,change_binary_string(Padded_data,Sub_format)};
			vl->
				Size =  erlang:size(Numb_check),
				Fsize = string:right(erlang:integer_to_list(Size),Fx_header_length,$0),							
				Final_list = [Fsize,change_binary_string(Numb_check,Sub_format)],
				{ok,Final_list}
		end.


%%for padding a binary up to a certain length with one string character or binary character
-spec pad_data_new(binary(),non_neg_integer(),binary(),left|right)->binary().
pad_data_new(Numb_check,Flength,Binary_char_pad,Pad_direction)->
		Pad_size = Flength-size(Numb_check),
		Pad_info = binary:copy(Binary_char_pad,Pad_size),
		case Pad_direction of
			right ->
				<< Pad_info/binary,Numb_check/binary >>;
			left ->
				<< Numb_check/binary,Pad_info/binary >>
		end.


%%this is a special setting for setting the mti of a message
-spec set_mti(Iso_Map::map(),Fld_val::term())->{ok,map()}.
set_mti(Iso_Map,Fld_val)->
		{ok,maps:put(mti,Fld_val,Iso_Map)}.


%% @doc this is for setting a particular field in the message or an mti
%% field will have to be validated and then after field is validated an entry is created as a map for it 
%%padding may be added to the field depending on the type of field as well as if its fixed or vlength
-spec set_field(Iso_Map::map(),Fld_num::pos_integer()|mti ,Fld_val::term())->{ok,map()}.
set_field(Iso_Map,Fld_num,Fld_val)->
		case Fld_num of 
			mti ->
				{ok,maps:put(mti,Fld_val,Iso_Map)};
			_ ->					
				{ok,maps:put(Fld_num,Fld_val,Iso_Map)}
		end.


%%this is for accepting a list containing the various fields and then creating an creating an output map 
%%which can be fed into the pack function
%%it can also also throw an exception if input data was of the wrong format for an individual field
-spec set_field_list(List::list())->map().
set_field_list(List)->
		First_map = maps:new(),
		lists:foldl(
		fun({Key,Value},Acc)->
			{ok,Map_new_Accum} = set_field(Acc,Key,Value),
			Map_new_Accum
		end,First_map,List).


%% @doc this is for getting a particular field in an iso message back
-spec get_field(Fld_num::pos_integer()|binary(),Iso_Map::map())->{ok,term()}|error.
get_field(Fld_num,Iso_Map)->
		Val_field = maps:get(Fld_num,Iso_Map,error),
		case Val_field of
			error ->
				error;
			_ ->
				{ok,Val_field}
		end.


%%for getting the final size of the message to be sent 
%%-spec get_size_send(binary(),binary()|list(),list())->non_neg_integer().
-spec get_size_send(iolist(),non_neg_integer())->list(). 
get_size_send(Fields_iolist,Length_max_size)->
	Final_length = erlang:iolist_size(Fields_iolist),
	string:right(erlang:integer_to_list(Final_length),Length_max_size,$0).
