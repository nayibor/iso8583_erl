%%%
%%% @doc iso8583_ascii module.
%%%<br>this module is responsible for processing string iso messages  format</br>
%%%<br>currently supports postillion message formats</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_ascii).

-export([unpack/3,pack/2,set_field/4,get_field/2,pad_data/3,process_data_element/3]).

-define(MTI_SIZE,4).
-define(BH,16).%%%byte header in bits.so a 2 byte header will be 16 at the end of the day

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
-spec unpack(list,atom(),list())->map().
unpack(list,Module_process,Rest)-> 
		Bin_message = erlang:list_to_binary(Rest),
		process_binary(Bin_message,Module_process).


%% @doc this function is used to derive various fields given an iso message and the message area(iso 1987,1992,2002,postillion,ascii subfield etc .. works with ascii )
%% can be used for getting various iso message fields as well as getting subfields out of an iso message
%%all data needed to calculate bitmamp should be part of input to this function 
-spec process_binary(binary(),atom())->map().
process_binary(Bin_message,Module_process)->
		<<One_dig/integer>> = binary_part(Bin_message,4,1),
		Bitsize = case  binary_part(convert_base_pad(One_dig,8,<<"0">>),0,1) of
							<<"0">> -> 8;
							<<"1">> -> 16
				  end,		
		<<Mti:?MTI_SIZE/binary,Bitmap_Segment:Bitsize/binary,Rest/binary>> = Bin_message,
		Bit_mess = << << (convert_base_pad(One,8,<<"0">>))/binary >>  || <<One/integer>> <= Bitmap_Segment >>,
		Mti_map = maps:put(<<"mti">>,Mti,maps:new()),
		Map_Init = maps:put(<<"bit">>,Bitmap_Segment,Mti_map),
		Result_process = process_data_element(Bit_mess,Rest,Module_process),
		maps:merge(Result_process,Map_Init).
		

%%for processing the message given the bitmap and the binary containing the data elements
-spec process_data_element(binary(),binary(),atom())->map().
process_data_element(Bitmap,Data_binary,Module_process)->
		Map_Init = maps:new(),
		OutData = fold_bin(
			 fun(<<X:1/binary, Rest_bin/binary>>, {Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= <<"1">> ->
					{_,Flength,Fx_var_fixed,Fx_header_length,_} = Module_process:get_spec_field(Current_index_in),
					Data_index = case Fx_var_fixed of
						fx -> 
							Data_element_fx = binary:part(Data_for_use_in,Index_start_in,Flength),
							New_Index_fx = Index_start_in+Flength,
							{Data_element_fx,New_Index_fx};
						vl ->
							Header = binary:part(Data_for_use_in,Index_start_in,Fx_header_length),
							Header_value = erlang:binary_to_integer(Header),
							Start_val = Index_start_in + Fx_header_length,
							Data_element_vl = binary:part(Data_for_use_in,Start_val,Header_value),
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
			end, {Data_binary,0,2,Map_Init},Bitmap),
		{_,_,_,Fldata} = OutData,
		Fldata.


%% @doc marshalls a message to be sent.
%%pack all the differnt elements in a message into
-spec pack(Message_Map::map(),Module_process::atom)->iolist().
pack(Message_Map,Module_process)->
		io:format("message_map is ~p",[Message_Map]),
		Process_value = 
		fun(Field_key,Acc={Bitmap,Bit_exist_secondary,Iso_Fields_Binary})->
			case maps:get(Field_key,Message_Map,error) of
				error ->
					Check_secondary = Field_key =< 64,
					case Check_secondary of
						true ->
							New_Bitmap = << Bitmap/binary,0 >>,
							{New_Bitmap,Bit_exist_secondary,Iso_Fields_Binary};
						false ->
							{Bitmap,Bit_exist_secondary,Iso_Fields_Binary}
					end;
				Value ->
					 Check_secondary = Field_key >= 65 andalso Bit_exist_secondary =:= false,
					 case Check_secondary of 
						true ->
							New_Bitmap = << 1, Bitmap/binary,1 >>,
							New_Iso_Fields_Binary = << Iso_Fields_Binary/binary,Value/binary >>,
							{New_Bitmap,true,New_Iso_Fields_Binary};
						false ->
							New_Bitmap = << Bitmap/binary,1>>,
							New_Iso_Fields_Binary = << Iso_Fields_Binary/binary,Value/binary >>,
							{New_Bitmap,Bit_exist_secondary,New_Iso_Fields_Binary}
					end
			end
		end,
		{Bitmap_final,_,Iso_Fields_Binary} = lists:foldl(Process_value,{<<>>,false,<<>>},lists:seq(2,128)),
		Mti = maps:get(mti,Message_Map),
		Final_payload = << Mti/binary,Bitmap_final/binary,Iso_Fields_Binary/binary >>,
	    Final_size = size(Final_payload),
		Final_list = [<<Final_size:?BH>>,Final_payload].
		
		


%%this will be used for formatting the data which is sent 
%%it is done at the setting stage
%%it checks if data is of the correct length and type for numbers and simple strings and binaries
%%%also adds paddings and as well as headers to the various values
%%not full featured but just enough to make it work
%%
-spec format_data(integer()|mti,term(),atom())->{ok,term()}|{error,term()}.
format_data(Key,Value,Module_process)->
	{Ftype,Flength,Fx_var_fixed,Fx_header_length,_}  = Module_process:get_spec_field(Key),
	io:format("~ndata spec is ~p",[{Ftype,Flength,Fx_var_fixed,Fx_header_length}]),
	case Ftype of
		n ->  %%input will be number
			Numb_check = 
				case {erlang:is_integer(Value),erlang:is_float(Value)} of
					{true,_}->
						 erlang:integer_to_binary(Value);
					{_,true}->
						erlang:float_to_binary(Value);
					{_,_}->
						{error,format_number_wrong}
				end,
			pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,<<"0">>);
		b ->  %%  input will be binary
			Numb_check = Value,
			pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,<<" ">>);
		ans -> %% input will be alphnumberic string
			Numb_check = erlang:list_to_binary(Value),
			pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,<<" ">>);
		ns ->  %% input will be numeric and special character string
			Numb_check = erlang:list_to_binary(Value),
			pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,<<" ">>);
		hex -> %% input will be hexadecimal string
			Numb_check = erlang:list_to_binary(Value),
			pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,<<" ">>)
	end.



%%for padding various fields based on whether its a variable length field or a fixed length field
pad_data_check(Fx_var_fixed,Fx_header_length,Flength,Numb_check,Binary_char_pad)->
	Status_check = Flength >= erlang:size(Numb_check),
	case {Status_check,Fx_var_fixed} of 
		{true,fx}->
			io:format("~ndata is ~p",[Numb_check]),
			Size = erlang:size(Numb_check),
			Padded_data = pad_data(Numb_check,Flength,Binary_char_pad),
			{ok,Padded_data};
		{true,vl}->
			Size = erlang:size(Numb_check),
			Fsize = pad_data(erlang:integer_to_binary(Size),Fx_header_length,Binary_char_pad),
			Final_binary = << Fsize/binary,Numb_check/binary>>,
			{ok,Final_binary};
		{false,_}->
			{error,error_length}
	end.



%%this is a special setting for setting the mti of a message
-spec set_field(Iso_Map::map(),Fld_num::pos_integer() ,Fld_val::term(),Module_process::atom)->{ok,map()}|{error,term()}.
set_field(Iso_Map,mti,Fld_val,Module_process)->
		Resp = format_data(0,Fld_val,Module_process),
		case Resp of
			{ok,Val} ->
				New_iso_map = maps:put(mti,Val,Iso_Map),
				{ok,New_iso_map};
			Result = {error,Reason}->
				Result
		end;


%% @doc this is for setting a particular field in the message or an mti
%% field will have to be validated and then after field is validated an entry is created as a map for it 
%%padding may be added to the field depending on the type of field as well as if its fixed or vlength
set_field(Iso_Map,Fld_num,Fld_val,Module_process)->
		Resp = format_data(Fld_num,Fld_val,Module_process),
		case Resp of
			{ok,Val} ->
				New_iso_map = maps:put(Fld_num,Val,Iso_Map),
				{ok,New_iso_map};
			Result = {error,Reason}->
				Result
		end.


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
