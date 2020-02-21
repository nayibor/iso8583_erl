%%%
%%% @doc iso8583_ascii module.
%%%<br>this module is responsible for processing string iso messages  format</br>
%%%<br>currently supports postillion message formats</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_ascii).

-export([unpack/2,pack/2,set_field/4,set_field_list/2,set_mti/4,get_field/2,pad_data/3,process_data_element/4,create_bitmap/2,get_bitmap_subs/3,get_size/2,convert_base_pad/3]).


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


%% @doc this is for padding a binary up to a length of N digits with a binary character
%%mostly used in the bitmap
%%pad character size <2
-spec pad_data(binary(),integer(),binary())->binary().
pad_data(Bin,Number,Character)when is_binary(Bin),is_integer(Number),Number > 0,is_binary(Character),size(Character)<2 -> pad_data(Bin,Number,Character,Number-size(Bin)).
pad_data(Bin,Number,Character,Counter) when Counter > 0 -> pad_data(<<Character/binary,Bin/binary>>,Number,Character,Counter-1);
pad_data(Bin,_Number,_Character,Counter) when Counter =< 0 -> Bin.


%% @doc  integer binary containing bitmap
-spec convert_base(integer())->binary().
convert_base(Data_Base_10)->
		erlang:integer_to_binary(Data_Base_10,2).
				
	  	
%% @doc this converts data between bases and also pads the data  for our purposes
-spec convert_base_pad(integer(),integer(),binary())->binary().
convert_base_pad(Data_Base_10,Number_pad,Pad_digit)->
        Data_base2 = convert_base(Data_Base_10),
		pad_data(Data_base2,Number_pad,Pad_digit).


%% @doc this part accepts a list iso message with the header removed and extracts the mti,bitmap,data elements into a map object 
%% it also accepts a module which will be used for getting the specifications for the message
%% exceptions can be thrown here if the string for the message hasnt been formatted well but they should be caught in whichever code is calling the system 
%%the data is first converted into a binary before the processing is done . much faster and uses less memory than using lists
-spec unpack(list(),atom())->map().
unpack(Rest,Module_process)-> 
		Bin_message = erlang:list_to_binary(Rest),
		process_binary(Bin_message,Module_process).


%% @doc this function is used to derive various fields given an iso message and the message area(iso 1987,1992,2002,postillion,ascii subfield etc .. works with ascii )
%% can be used for getting various iso message fields as well as getting subfields out of an iso message
%%all data needed to calculate bitmamp should be part of input to this function 
-spec process_binary(binary(),atom())->map().
process_binary(Bin_message,Module_process)->
		Bitmap_type = Module_process:get_bitmap_type(),
		{Mti,Bit_mess,Bitmap_Segment,Rest} = get_bitmap_subs(Bitmap_type,Bin_message,Module_process),
		<<_:1/binary,Real_bitmap/binary>> = Bit_mess,
		Mti_map = maps:put(mti,Mti,maps:new()),
		Map_Init = maps:put(bit,Bitmap_Segment,Mti_map),
		Result_process = process_data_element(Real_bitmap,2,Rest,Module_process),
		maps:merge(Result_process,Map_Init).


%%for processing the message given the bitmap and the binary containing the data elements
-spec process_data_element(binary(),integer(),binary(),atom())->map().
process_data_element(Bitmap,Index_start,Data_binary,Module_process)->
		Map_Init = maps:new(),
		OutData = fold_bin(
			 fun(<<X:1/binary, Rest_bin/binary>>, {Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= <<"1">> ->
					{Data_type,Flength,Fx_var_fixed,Fx_header_length,_} = Module_process:get_spec_field(Current_index_in),
					Data_index = case Fx_var_fixed of
						fx -> 
							Data_element_fx_raw = binary:part(Data_for_use_in,Index_start_in,Flength),
							Data_element_vl = get_data_element(Data_element_fx_raw,Data_type),
							New_Index_vl = Index_start_in+Flength,
							{Data_element_vl,New_Index_vl};
						vl ->
							Header = binary:part(Data_for_use_in,Index_start_in,Fx_header_length),
							Header_value = erlang:binary_to_integer(Header),
							Start_val = Index_start_in + Fx_header_length,
							Data_element_vl_raw = binary:part(Data_for_use_in,Start_val,Header_value),
							Data_element_vl = get_data_element(Data_element_vl_raw,Data_type),
							New_Index_vl = Start_val+Header_value,
							{Data_element_vl,New_Index_vl}
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


%%for deriving native value of unpacked data
-spec get_data_element(binary(),atom())->number()|list()|binary().
get_data_element(Data_value,Type)->
		case Type of
			n->
				
				bin_to_num(Data_value);
			b->
				Data_value;
			ans->
				erlang:binary_to_list(Data_value);
			ns->
				erlang:binary_to_list(Data_value);
			hex->
				erlang:binary_to_list(Data_value)
		end.


%%for converting a number to a float or an integer based on input 
-spec bin_to_num(binary())->float()|integer().
bin_to_num(Bin) ->
	    N = binary_to_list(Bin),
	    case string:to_float(N) of
	        {error,no_float} -> list_to_integer(N);
	        {F,_Rest} -> F
	    end.


%% @doc marshalls a message to be sent.
%%pack all the differnt elements in a message into an iolist
-spec pack(Message_Map::map(),Module_process::atom())->iolist().
pack(Message_Map,Module_process)->
		Process_value = 
		fun(Field_key,{Bitmap,Iso_Fields})->
			case maps:get(Field_key,Message_Map,error) of
				error ->
					New_Bitmap = << Bitmap/binary,0 >>,
					{New_Bitmap,Iso_Fields};
				Value ->
					New_Bitmap = << Bitmap/binary,1>>,
					New_Iso_Fields = [Value|Iso_Fields],
					{New_Bitmap,New_Iso_Fields}
			end
		end,
		{Bitmap_final,Iso_Fields_Binary} = lists:foldl(Process_value,{<<>>,[]},lists:seq(2,128)),
		Pred = fun(Key,_) -> erlang:is_integer(Key) andalso (Key >= 65)  andalso (Key =< 128)  end,
		Secondary_bitmap_flag = maps:filter(Pred,Message_Map),
		Bitmap_final_bit = 
		case erlang:map_size(Secondary_bitmap_flag) of
			0->
				<< 0,Bitmap_final/binary>>;
			_ ->
				<< 1,Bitmap_final/binary>>
		end,
		Bitmap_final_bit_list = create_bitmap(Module_process:get_bitmap_type(),Bitmap_final_bit),
		Mti = maps:get(mti,Message_Map),
		Fields_list = lists:reverse(Iso_Fields_Binary),
		[Mti,Bitmap_final_bit_list,Fields_list].


%% @doc for getting the bitmap,mti,Data fields 
-spec get_bitmap_subs(atom(),binary(),atom())-> tuple().
get_bitmap_subs(binary,Bin_message,Module_process)->
		{_,Flength,_,_,_} = Module_process:get_spec_field(1),
		<<One_dig/integer>> = binary_part(Bin_message,Flength,1),
		Bitsize = case  binary_part(convert_base_pad(One_dig,8,<<"0">>),0,1) of
							<<"0">> -> 8;
							<<"1">> -> 16
				  end,		
		<<Mti:Flength/binary,Bitmap_Segment:Bitsize/binary,Rest/binary>> = Bin_message,
		Bit_mess = << << (convert_base_pad(One,8,<<"0">>))/binary >>  || <<One>> <= Bitmap_Segment >>,
		{Mti,Bit_mess,Bitmap_Segment,Rest};


get_bitmap_subs(hex,Bin_message,Module_process)->
		{_,Flength,_,_,_} = Module_process:get_spec_field(1),
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



%%for creating the final bitmap
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


%%this will be used for formatting the data which is sent 
%%it is done at the setting stage
%%it checks if data is of the correct length and type for numbers and simple strings and binaries
%%%also adds paddings and as well as headers to the various values
%%not full featured but just enough to make it work
-spec format_data(integer()|mti,term(),atom())->{ok,term()}|{error,term()}.
format_data(Key,Value,Module_process)->
		{Ftype,Flength,Fx_var_fixed,Fx_header_length,_}  = Module_process:get_spec_field(Key),
		case Ftype of
			n ->  %%input will be number
				Numb_check = 
					case {erlang:is_integer(Value),erlang:is_float(Value)} of
						{true,_}->
							 erlang:integer_to_list(Value);
						{_,true}->
							erlang:float_to_list(Value,[{decimals,5},compact]);
						{_,_}->
							{error,format_number_wrong}
					end,
				pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,$0,string);
			b ->  %%  input will be binary
				Numb_check = Value,
				pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,<<" ">>,binary);
			ans -> %% input will be alphnumberic string
				Numb_check = Value,
				pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,$  ,string);
			ns ->  %% input will be numeric and special character string
				Numb_check = Value,
				pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,$  ,string);
			hex -> %% input will be hexadecimal string
				Numb_check = Value,
				pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,$ ,string)
		end.


%%for padding various fields based on whether its a variable length field or a fixed length field
-spec pad_data_check(fx|vl,integer(),integer(),binary()|list(),char()|atom()|binary(),atom())->{ok,list()}|{ok,binary()}|{error,term()}.
pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,Char_pad,Type)->
		case Type of 
			string ->
				Status_check = erlang:length(Numb_check) =< Flength ,
				case {Status_check,Fx_var_fixed} of 
						{true,fx}->
							Padded_data = pad_data_string_binary(string,Numb_check,Flength,Char_pad),
							{ok,Padded_data};
						{true,vl}->
									Size = erlang:length(Numb_check),
									Fsize = string:right(erlang:integer_to_list(Size),Fx_header_length,$0),
									Final_string = lists:append([Fsize,Numb_check]),
									{ok,Final_string};
						{false,_}->
							{error,error_length}
				end;
			binary ->
				Status_check = erlang:size(Numb_check) =< Flength ,
				case {Status_check,Fx_var_fixed} of 
						{true,fx}->
							Padded_data = pad_data_string_binary(binary,Numb_check,Flength,Char_pad),
							{ok,Padded_data};
						{true,vl}->
								Size =  erlang:size(Numb_check),
								Fsize = string:right(erlang:integer_to_list(Size),Fx_header_length,$0),							
								Final_binary = [Fsize,<<Numb_check/binary>>],
								{ok,Final_binary};
						{false,_}->
							{error,error_length}
				end
		end.


%%for padding a string or a binary up to a certain length with one string character or binary character
-spec pad_data_string_binary('string'|'binary',binary()|string(),non_neg_integer(),char()|<<_:8>>)->{ok,string()|binary()}.
pad_data_string_binary(string,Numb_check,Flength,Binary_char_pad)->
		string:right(Numb_check,Flength,Binary_char_pad);


pad_data_string_binary(binary,Numb_check,Flength,Binary_char_pad)->
		pad_data(Numb_check,Flength,Binary_char_pad).



%%accepts response of formatting of iso field and then  updates iso map if result is good
-spec format_create_map(integer()|mti,{ok,term()}|{error,term()},map())->{ok,map()}|{error,term()}.
format_create_map(Key,Resp,Old_iso_map)->
		case Resp of
			{ok,Val} ->
				New_iso_map = maps:put(Key,Val,Old_iso_map),
				{ok,New_iso_map};
			Result = {error,_}->
				Result
		end.


%%this is a special setting for setting the mti of a message
-spec set_mti(Iso_Map::map(),mti ,Fld_val::term(),Module_process::atom)->{ok,map()}|{error,term()}.
set_mti(Iso_Map,mti,Fld_val,Module_process)->
		Resp = format_data(1,Fld_val,Module_process),
		format_create_map(mti,Resp,Iso_Map).


%% @doc this is for setting a particular field in the message or an mti
%% field will have to be validated and then after field is validated an entry is created as a map for it 
%%padding may be added to the field depending on the type of field as well as if its fixed or vlength
-spec set_field(Iso_Map::map(),Fld_num::pos_integer()|mti ,Fld_val::term(),Module_process::atom)->{ok,map()}|{error,term()}.
set_field(Iso_Map,Fld_num,Fld_val,Module_process)->
		case Fld_num of 
			mti ->
				Resp = format_data(1,Fld_val,Module_process),	
				format_create_map(mti,Resp,Iso_Map);
			_ ->					
				Resp = format_data(Fld_num,Fld_val,Module_process),
				format_create_map(Fld_num,Resp,Iso_Map)
		end.


%%this is for accepting a list containing the various fields and then creating an creating an output map 
%%which can be fed into the pack function
%%it can also also throw an exception if input data was of the wrong format for an individual field
-spec set_field_list(List::list(),Module_process::atom())->map().
set_field_list(List,Module_process)->
		First_map = maps:new(),
		lists:foldl(
		fun({Key,Value},Acc)->
			{ok,Map_new_Accum} = set_field(Acc,Key,Value,Module_process),
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


%%for calculating size of bitmap or field list
-spec get_size(bitmap:atom(),binary()|list())->integer().
get_size(bitmap,Bitmap)->
		case {is_list(Bitmap),is_binary(Bitmap)} of 
			{true,_}->
				length(Bitmap);
			{_,true}->
				size(Bitmap)
		end;


get_size(field_list,Fields_list)->
	lists:foldl(fun(X,Acc)->
		case {is_list(X),is_binary(X)} of
			{true,_}->
				Acc+length(X);
			{_,true}->
				Acc+size(X)
		end
	  end,0,Fields_list).

