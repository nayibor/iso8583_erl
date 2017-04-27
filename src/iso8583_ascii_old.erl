%%%
%%% @doc yapp_test_ascii_marsh_jpos module.
%%%<br>this module is responsible for processing iso messages using iso1993 jpos message format</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_ascii_old).

-export([unpack/1]).

-define(MTI_SIZE,4).
-define(PRIMARY_BITMAP_SIZE,16).


%%this is for performing a binary fold kind of like a list fold
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



 
%%this is for padding a binary up to a length of N digits with a binary character
%%mostly used in the bitmap
%%pad character size <2

pad_data(Bin,Number,Character)when is_binary(Bin),is_integer(Number),Number > 0,is_binary(Character),size(Character)<2 -> pad_data(Bin,Number,Character,Number-size(Bin)).
pad_data(Bin,Number,Character,Counter) when Counter > 0 -> pad_data(<<Character/binary,Bin/binary>>,Number,Character,Counter-1);
pad_data(Bin,_Number,_Character,Counter) when Counter =< 0 -> Bin.

%%this is for creating correct interpretation of the bitmap for a binary 
convert_base(Data_Base_16)->
		Fth_base16 = erlang:binary_to_integer(Data_Base_16,16),
		erlang:integer_to_binary(Fth_base16,2).

%%this converts data between bases and also pads the data  for our purposes
convert_base_pad(Data_Base_16,Number_pad,Pad_digit)->
        Data_base2 = convert_base(Data_Base_16),
		pad_data(Data_base2,Number_pad,Pad_digit).




%% @doc this part accepts a message with the header removed and extracts the mti,bitmap,data elements into a map object 
%% exceptions can be thrown here if the string for the message hasnt been formatted well but they should be caught in whichever code is calling the system 
%%the data is first converted into a binary before the processing is done . much faster and uses less memory than list couterpart below	
unpack({binary,Rest})-> 
		 Bin_message = erlang:list_to_binary(Rest),
		 Fthdig = binary:part(Bin_message,4,1),
		 Pad_left_z_basetwo = convert_base_pad(Fthdig,4,<<"0">>),		 
		%%io:format("~n4base pad is ~p",[Pad_left_z_basetwo]),
		 Bitmap_test_num = binary:part(Pad_left_z_basetwo,0,1),
		 Bitmap_size = case Bitmap_test_num of
							<<"0">> -> 16;
							<<"1">> -> 32
						end,
		Bitmap_Segment = binary:part(Bin_message,?MTI_SIZE,Bitmap_size),
		Bitmap_transaction = << << (convert_base_pad(One,4,<<"0">>)):4/binary >>  || <<One:1/binary>> <= Bitmap_Segment >>,
		%%add bitmap as well as mti to map which holds data elements so they can help in processing rules 
		%%Mti_Data_Element = maps:from_list([{ftype,ans},{fld_no,0},{name,<<"Mti">>},{val_binary_form,binary:part(Bin_message,0,?MTI_SIZE)}]),
		%%Bitmap_Data_ELement = maps:from_list([{ftype,b},{fld_no,1},{name,<<"Bitmap">>},{val_binary_form,Bitmap_transaction},{val_hex_form,Bitmap_Segment}]),
		%%Map_Data_Element1 =  maps:put(<<"_mti">>,Mti_Data_Element,maps:new()), 
		%%Map_Data_Element = maps:put(0,Bitmap_Data_ELement,Map_Data_Element1),
		Map_Data_Element = maps:new(),
		Start_index = ?MTI_SIZE+?PRIMARY_BITMAP_SIZE,
		OutData = fold_bin(
			fun( <<X:1/binary, Rest_bin/binary>>, {Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= <<"1">> ->
					{_Ftype,Flength,Fx_var_fixed,Fx_header_length,_DataElemName} = yapp_test_ascii_def:get_spec_field(Current_index_in),
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
					%%NewData  = maps:from_list([{val_binary_form,Data_element}]),
					NewMap = maps:put(Current_index_in,Data_element,Map_out_list_in),
					Fld_num_out = Current_index_in + 1, 
					{Rest_bin,{Data_for_use_in,New_Index,Fld_num_out,NewMap}};
			   (<<X:1/binary, Rest_bin/binary>>, {Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= <<"0">> ->
					Fld_num_out = Current_index_in + 1,					
					{Rest_bin,{Data_for_use_in,Index_start_in,Fld_num_out,Map_out_list_in}}
			end, {Bin_message,Start_index,1,Map_Data_Element},Bitmap_transaction),
		{_,_,_,FlData} = OutData,
		%%io:format("message is ~p",[FlData]),
		FlData;


%% @doc this part accepts a message with the header removed and extracts the mti,bitmap,data elements into a map object 
%% exceptions can be thrown here if the string for the message hasnt been formatted well but they should be caught in whichever code is calling the system 
%%this part does this using a list and all data is processed as a list 
unpack({list,Rest})->
		
		Fthdig = [lists:nth(5,Rest)] ,
		Fth_base16 = list_to_integer(Fthdig,16),
		Fth_base2 = integer_to_list(Fth_base16,2),
		Pad_left_z_basetwo = string:right(Fth_base2,4,$0),
		Bitmap_test_num = lists:nth(1,Pad_left_z_basetwo),        
		Bitmap_size = case Bitmap_test_num of
						48 -> 16;
						49 -> 32
						end,
		
		Bitmap_Segment = lists:sublist(Rest,?MTI_SIZE+1,Bitmap_size),
		Fun_ret_bitmap_binary_elem = fun(X)->
										Sing_item = list_to_integer([X],16),
										Integer_sing_item = integer_to_list(Sing_item,2), 	
										string:right(Integer_sing_item,4,$0)
									  end ,
		Bitmap_list_raw = lists:map(Fun_ret_bitmap_binary_elem,Bitmap_Segment),
		Bitmap_transaction = lists:flatten(Bitmap_list_raw),
		
		%%add bitmap as well as mti to map which holds data elements so they can help in processing rules 
		Mti_Data_Element = maps:from_list([{ftype,ans},{fld_no,0},{name,<<"Mti">>},{val_list_form,lists:sublist(Rest,?MTI_SIZE)}]),
		Bitmap_Data_ELement = maps:from_list([{ftype,b},{fld_no,1},{name,<<"Bitmap">>},{val_binary_form,Bitmap_transaction},{val_list_form,lists:sublist(Rest,?MTI_SIZE+1,Bitmap_size)}]),
		Map_Data_Element1 =  maps:put(<<"_mti">>,Mti_Data_Element,maps:new()), 
		Map_Data_Element = maps:put(<<"_bitmap">>,Bitmap_Data_ELement,Map_Data_Element1),
		Start_index = ?MTI_SIZE+?PRIMARY_BITMAP_SIZE+1,
		%%io:format("~nkeys and values so far are ~p",[Map_Data_Element]),

		OutData = lists:foldl(fun(X,_Acc={Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= 49->						
								    {Ftype,Flength,Fx_var_fixed,Fx_header_length,DataElemName}=yapp_test_ascii_def:get_spec_field(Current_index_in),
									case Fx_var_fixed of
										fx -> 
											Data_Element = lists:sublist(Data_for_use_in,Index_start_in,Flength),
											New_Index = Index_start_in+Flength ;	
										vl ->
											Vl_value = list_to_integer(lists:sublist(Data_for_use_in,Index_start_in,Fx_header_length)),
											Start_val = Index_start_in + Fx_header_length , 										
											Data_Element = lists:sublist(Data_for_use_in,Start_val,Vl_value),
											New_Index = Start_val+Vl_value
									end,
									NewData  = maps:from_list([{ftype,Ftype},{fld_no,Current_index_in},{name,DataElemName},{val_list_form,Data_Element}]),
									NewMap = maps:put("_"++integer_to_list(Current_index_in),NewData,Map_out_list_in),
									Fld_num_out = Current_index_in + 1,
									%%io:format("~nkeys and values so far are ~p",[NewMap]),
									{Data_for_use_in,New_Index,Fld_num_out,NewMap};
								(X,_Acc={Data_for_use_in,Index_start_in,Current_index_in,Map_out_list_in}) when X =:= 48->
									Fld_num_out = Current_index_in + 1,						
									{Data_for_use_in,Index_start_in,Fld_num_out,Map_out_list_in}
							 end,
					   {Rest,Start_index,1,Map_Data_Element},Bitmap_transaction),
		
		{_,_,_,FlData} = OutData,
		%%io:format("~nkeys and values so far are ~p",[FlData]),
		FlData.



%% @doc marshalls a message to be sent 
-spec pack(Message_Map::map())->[pos_integer()].
pack(_Message_Map)->
		ok.


%% @doc this is for setting a particular field in the message
-spec set_field(map(),pos_integer(),pos_integer()|[pos_integer()]|binary())->map().
set_field(Map_Message,Fld_num,Fld_val)->
		ok.
		
		
%%have to add function which will set sub field		
%%have to add a function which will get mti 
%% possible give mti of response message based on request 
