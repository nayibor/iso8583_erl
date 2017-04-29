%%%
%%% @doc iso8583_ascii module.
%%%<br>this module is responsible for processing string iso messages  format</br>
%%%<br>currently supports postillion message formats</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_ascii).

-export([unpack/3]).

-define(MTI_SIZE,4).


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


%% @doc this part accepts a postilion list iso message with the header removed and extracts the mti,bitmap,data elements into a map object 
%% exceptions can be thrown here if the string for the message hasnt been formatted well but they should be caught in whichever code is calling the system 
%%the data is first converted into a binary before the processing is done . much faster and uses less memory than using lists
-spec unpack(list,post,binary())->map().
unpack(list,post,Rest)-> 
		%%io:format("~ninit message is ~s",[Rest]),
		Bin_message = erlang:list_to_binary(Rest),
		process_binary(Bin_message,post).


%% @doc this function is used to derive various fields given an iso message and the message area(iso 1987,1992,2002,postillion,ascii subfield etc .. works with ascii )
%% can be used for getting various iso message fields as well as getting subfields out of an iso message
%%all data needed to calculate bitmamp should be part of input to this function 
-spec process_binary(binary(),atom())->map().
process_binary(Bin_message,Message_Area)->
		{Btmptrans,Msegt,Spec_fun,Map_Init} = case Message_Area of
								post ->
									<<One_dig/integer>> = binary_part(Bin_message,4,1),
									Bitsize = case  binary_part(convert_base_pad(One_dig,8,<<"0">>),0,1) of
														<<"0">> -> 8;
														<<"1">> -> 16
											  end,		
									<<Mti:?MTI_SIZE/binary,Bitmap_Segment:Bitsize/binary,Rest/binary>> = Bin_message,
									Bit_mess = << << (convert_base_pad(One,8,<<"0">>))/binary >>  || <<One/integer>> <= Bitmap_Segment >>,
									Mti_map = maps:put(<<"mti">>,Mti,maps:new()),
									Bit_map = maps:put(<<"bit">>,Bitmap_Segment,Mti_map),
									{Bit_mess,<<Bitmap_Segment/binary,Rest/binary>>,fun(Index_f)->iso8583_ascii_post:get_spec_field(Index_f)end,Bit_map} 
											  end,	
		OutData = fold_bin(
			 fun(<<X:1/binary, Rest_bin/binary>>, {Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= <<"1">> ->
					{_Ftype,Flength,Fx_var_fixed,Fx_header_length,_DataElemName} = Spec_fun(Current_index_in),
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
					%%io:format("~nso far ~p and field_num is ~p",[NewMap,Current_index_in]),
					Fld_num_out = Current_index_in + 1, 
					{Rest_bin,{Data_for_use_in,New_Index,Fld_num_out,NewMap}};
				(<<X:1/binary, Rest_bin/binary>>, {Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= <<"0">>,Current_index_in =:=1,Message_Area =:=post->
					New_Index_fx = Index_start_in+8,
					Fld_num_out = Current_index_in + 1,					
					{Rest_bin,{Data_for_use_in,New_Index_fx,Fld_num_out,Map_out_list_in}};
			    (<<X:1/binary, Rest_bin/binary>>, {Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= <<"0">> ->
					Fld_num_out = Current_index_in + 1,					
					{Rest_bin,{Data_for_use_in,Index_start_in,Fld_num_out,Map_out_list_in}}
			end, {Msegt,0,1,Map_Init},Btmptrans),
		{_,_,_,Fldata} = OutData,
		%%io:format("~nmap is ~p",[Fldata]),
		Fldata.

%% @doc marshalls a message to be sent 
-spec pack(Message_Map::map())->[pos_integer()].
pack(_Message_Map)->
		ok.


%% @doc this is for setting a particular field in the message
-spec set_field(Iso_Map::map(),Fld_num::pos_integer(),Fld_val::term())->{ok,map()}|{error,binary()}.
set_field(_Iso_Map,_Fld_num,_Fld_val)->
		{ok,<<>>}.
		
%% @doc this is for getting a particular field in the message
-spec get_field(Fld_num::pos_integer(),Iso_Map::map())->{ok,binary()}|{error,binary()}.
get_field(Fld_num,Iso_Map)->
		{ok,<<>>}.
		
		
%%have to add function which will set sub field		
%%have to add a function which will get mti 
%% possible give mti of response message based on request 
