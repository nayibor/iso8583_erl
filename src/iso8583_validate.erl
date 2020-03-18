%%%
%%% @doc iso8583_ascii module.
%%%<br>this module is performing various validations</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_validate).

-export([validate_data/3,perform_validation/3]).


%% @doc this is for validating an iso8583 message for incoming messages based on a specification
%% @doc De_type,Length_field,Fl_vl,Header_length,Format
-spec validate_data(map(),map(),out|in)->{{ok,map()},{error,list()}}.
validate_data(Data_map,Specification_map,Out_or_in)->
	Map_fold = 
	fun(Key_iso,Value_iso,{{ok,Map_result},{error,Error_list}})when Key_iso =/= mti andalso erlang:is_integer(Key_iso) andalso Key_iso >= 2  andalso Key_iso =< 128->
	    {Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format,Pad_info} = iso8583_process:get_spec_field(Key_iso,Specification_map),
		Result_validation = validate_data_sub({Value_iso,Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format,Pad_info,Out_or_in}),
		case Result_validation of
			true ->
				New_result_map = maps:put(Key_iso,Value_iso,Map_result),
				{{ok,New_result_map},{error,Error_list}};
			{error,Result} ->
				{{ok,Map_result},{error,[{Key_iso,{Value_iso,Result}}|Error_list]}}
		end;
	  (Key_iso,Value_iso,{{ok,Map_result},{error,Error_list}})when Key_iso =:= mti->
		 List_valid_mti = iso8583_process:get_spec_field(valid_mti,Specification_map),
		 Result = lists:member(Value_iso,List_valid_mti),
		 case Result of 
			true ->
				New_result_map = maps:put(Key_iso,Value_iso,Map_result),
				{{ok,New_result_map},{error,Error_list}};
			false ->
				{{ok,Map_result},{error,[{Key_iso,{Value_iso,<<"Invalid Mti">>}}|Error_list]}}
		 end;	
	  (_,_,Acc_in)->
		 Acc_in
	end,		
	maps:fold(Map_fold,{{ok,maps:new()},{error,[]}},Data_map).



%% @doc for validating  a particular element based on fixed length field 
-spec validate_data_sub(tuple())->{error,binary()}|true.
validate_data_sub({Data,Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format,{none,_},out})when  
	erlang:size(Data) =:= Flength andalso erlang:size(Data) > 0->
		perform_validation(Data,Data_type,Sub_format);


%% @doc for validating  a particular element based variable length field
validate_data_sub({Data,Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format,{_,_},out})when  
     erlang:size(Data) =< Flength andalso erlang:size(Data) > 0->
		perform_validation(Data,Data_type,Sub_format);
		
%% @doc for validating  a particular element based on fixed length field 
validate_data_sub({Data,Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format,{none,_},in})when  
	erlang:size(Data) =:= Flength andalso erlang:size(Data) > 0->
		perform_validation(Data,Data_type,Sub_format);


validate_data_sub(_)->
	{error,<<"Length Error">>}.	


%% @doc for performing validation on each data element  for a number
-spec perform_validation(binary(),list(),list())-> ok | {error,binary()}.
perform_validation(Data,"N","N")  ->
	run_regex(Data,"^[0-9]*$",[]);



%% @doc for performing validation on each data element for a signed number beginning with C or D
perform_validation(Data,"N","SN")->
	run_regex(Data,"^[C|D][0-9]*$",[]);	


%%for performinig validaton for a floating point numberic digit without  +\-
perform_validation(Data,"N","NF")->
	run_regex(Data,"^[0-9]*\.?[0-9]+$",[]);
	

%%for performinig validaton for a signed floating point numeric digit number beginning wiht a credit/debit without +|-
perform_validation(Data,"N","SNF")->
	run_regex(Data,"^[C|D][0-9]*\.?[0-9]+$",[]);

%%fr performing regex of alphabets
perform_validation(Data,"A","A")->
	run_regex(Data,"^[a-zA-Z]*$",[]);

%% for performing alpha numeric checks
perform_validation(Data,"AN","AN")->
	run_regex(Data,"^[a-zA-Z0-9]*$",[]);


%% for performing alpha numeric checks
perform_validation(Data,"ANS","ANS")->
	run_regex(Data,"",[]);


perform_validation(Data,"S","S")->
	run_regex(Data,"",[]);


perform_validation(Data,"NS","NS")->
	run_regex(Data,"",[]);


%%binary characters are checked only for size 
perform_validation(Data,"B","B")->
	true;


perform_validation(Data=[MM,DD,HH,MM,SS],"N","MMDDhhmmss")->
	run_regex(Data,"",[]);

perform_validation(Data,"N","hhmmss")->
	run_regex(Data,"",[]);


perform_validation(Data,"N","YYMMddhhmmss")->
	run_regex(Data,"",[]);

perform_validation(Data,"N","MMDD")->
	run_regex(Data,"",[]);

perform_validation(Data,"N","YYMM")->
	run_regex(Data,"",[]);
	
perform_validation(Data,"N","YYMMDD")->
	run_regex(Data,"",[]);


perform_validation(_,_,_)->
	{error,<<"Format Error">>}.




run_regex(Data,Regex_Expression,Options)->
  case re:run(Data, Regex_Expression,Options) of
      match -> true;
      {match, _} -> true;
      _ -> {error,<<"Format_error">>}
  end.
