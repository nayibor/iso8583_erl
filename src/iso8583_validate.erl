%%%
%%% @doc iso8583_ascii module.
%%%<br>this module is performing various validations</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_validate).

-export([validate_data/3,perform_validation/2]).


%% @doc this is for validating an iso8583 message for incoming messages based on a specification
%% @doc De_type,Length_field,Fl_vl,Header_length,Format
-spec validate_data(map(),map(),out|in)->{{ok,map()},{error,list()}}.
validate_data(Data_map,Specification_map,Out_or_in)->
	Map_fold = 
	fun(Key_iso,Value_iso,{{ok,Map_result},{error,Error_list}})when Key_iso =/= mti andalso erlang:is_integer(Key_iso) andalso Key_iso >= 2  andalso Key_iso =< 128->
	    {Flength,Fx_var_fixed,Fx_header_length,Sub_format,Pad_info} = iso8583_erl:get_spec_field(Key_iso,Specification_map),
		Result_validation = validate_data_sub({Value_iso,Flength,Fx_var_fixed,Fx_header_length,Sub_format,Pad_info,Out_or_in}),
		case Result_validation of
			true ->
				New_result_map = maps:put(Key_iso,Value_iso,Map_result),
				{{ok,New_result_map},{error,Error_list}};
			{error,Result} ->
				{{ok,Map_result},{error,[{Key_iso,{Value_iso,Result}}|Error_list]}}
		end;
	  (Key_iso,Value_iso,{{ok,Map_result},{error,Error_list}})when Key_iso =:= mti->
				New_result_map = maps:put(Key_iso,Value_iso,Map_result),
				{{ok,New_result_map},{error,Error_list}};
	  (_,_,Acc_in)->
		 Acc_in
	end,		
	maps:fold(Map_fold,{{ok,maps:new()},{error,[]}},Data_map).


%% @doc  input data length must be equal as field length when fixed length and no padding and outgoing
-spec validate_data_sub(tuple())->{error,binary()}|true.
validate_data_sub({Data,Flength,fx,Fx_header_length,Sub_format,{none,_},out})when  
	erlang:size(Data) =:= Flength andalso erlang:size(Data) > 0 ->
		perform_validation(Data,Sub_format);


%% @doc input data length can be less than or equal to field length when variable length and no padding and outgoing
validate_data_sub({Data,Flength,vl,Fx_header_length,Sub_format,{none,_},out})when  
	erlang:size(Data) =< Flength andalso erlang:size(Data) > 0 ->
		perform_validation(Data,Sub_format);


%% @doc input data length can be less than or equal to field length when  fixed length and padded and outgoing
validate_data_sub({Data,Flength,fx,Fx_header_length,Sub_format,{_,_},out})when  
	erlang:size(Data) =< Flength andalso erlang:size(Data) > 0 ->
		perform_validation(Data,Sub_format);


%% @doc incoming data length must be equal as field length when fixed length and no padding and incoming
validate_data_sub({Data,Flength,fx,Fx_header_length,Sub_format,{none,_},in})when  
     erlang:size(Data) =:= Flength andalso erlang:size(Data) > 0->
		perform_validation(Data,Sub_format);


%% @doc incoming data length can be less than or equal to field length when variable length and no padding and incoming
validate_data_sub({Data,Flength,vl,Fx_header_length,Sub_format,{none,_},in})when  
     erlang:size(Data) =< Flength andalso erlang:size(Data) > 0->
		perform_validation(Data,Sub_format);


%% @doc incoming data length must be equal as field length when fixed length and padding and incoming
validate_data_sub({Data,Flength,fx,Fx_header_length,Sub_format,{_,_},in})when  
     erlang:size(Data) =:= Flength andalso erlang:size(Data) > 0->
		perform_validation(Data,Sub_format);



validate_data_sub(_)->
	{error,<<"Length Error">>}.	


%% @doc for performing validation on each data element  for a number
-spec perform_validation(binary(),list())-> ok | {error,binary()}.
perform_validation(Data,"N")  ->
	run_regex(Data,"^[0-9]*$",[]);



%% @doc for performing validation on each data element for a signed number beginning with C or D
perform_validation(Data,"SN")->
	run_regex(Data,"^[C|D][0-9]*$",[]);	


%%for performinig validaton for a floating point numberic digit without  +\-
perform_validation(Data,"F")->
	run_regex(Data,"^[0-9]*\.?[0-9]+$",[]);
	

%%for performinig validaton for a signed floating point numeric digit number beginning wiht a credit/debit without +|-
perform_validation(Data,"SF")->
	run_regex(Data,"^[C|D][0-9]*\.?[0-9]+$",[]);

%%fr performing regex of alphabets
perform_validation(Data,"A")->
	run_regex(Data,"^[a-zA-Z]*$",[]);

%% for performing alpha numeric checks
perform_validation(Data,"AN")->
	run_regex(Data,"^[a-zA-Z0-9]*$",[]);


%% for performing alpha numeric checks
perform_validation(Data,"ANS")->
	run_regex(Data,"",[]);


%% for performing special character checks
perform_validation(Data,"S")->
	run_regex(Data,"",[]);


%% for performing numeric or special characters
perform_validation(Data,"NS")->
	run_regex(Data,"",[]);


%%binary characters are checked only for size 
perform_validation(Data,"B")->
	true;


perform_validation(Data=[MM,DD,HH,MM,SS],"MMDDhhmmss")->
	run_regex(Data,"",[]);

perform_validation(Data,"hhmmss")->
	run_regex(Data,"",[]);


perform_validation(Data,"YYMMddhhmmss")->
	run_regex(Data,"",[]);

perform_validation(Data,"MMDD")->
	run_regex(Data,"",[]);

perform_validation(Data,"YYMM")->
	run_regex(Data,"",[]);
	
perform_validation(Data,"YYMMDD")->
	run_regex(Data,"",[]);


perform_validation(_,_)->
	{error,<<"Format Error">>}.




run_regex(Data,Regex_Expression,Options)->
  case re:run(Data, Regex_Expression,Options) of
      match -> true;
      {match, _} -> true;
      _ -> {error,<<"Format_error">>}
  end.
