%%%
%%% @doc iso8583_ascii module.
%%%<br>this module is performing various validations</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_validate).

-export([validate_data/2,perform_validation/3]).


%% @doc this is for validating an iso8583 message for incoming messages based on a specification
%% @doc De_type,Length_field,Fl_vl,Header_length,Format
-spec validate_data(map(),map())->{{ok,map()},{error,list()}}.
validate_data(Data_map,Specification_map)->
	Map_fold_iso_function = 
	fun(Key_iso,Value_iso,Acc_in = {{ok,Map_result},{error,Error_list}})when erlang:is_integer(Key_iso) andalso (Key_iso >= 2)  andalso (Key_iso =< 128)->
		    {Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format} = iso8583_ascii:get_spec_field(Key_iso,Specification_map),
			Result_validation = validate_data_sub({Value_iso,Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format}),
			case Result_validation of
				ok ->
					New_result_map = maps:put(Key_iso,Value_iso,Map_result),
					{{ok,New_result_map},{error,Error_list}};
				{error,Result} ->
					{{ok,Map_result},{error,[{Key_iso,Result}|Error_list]}}
			end;
		(_,_,Acc_in)->
			Acc_in
		end,		
	maps:fold(Map_fold_iso_function,{ok,maps:new(),{error,[]}},Data_map).




validate_data_sub({Data,Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format})when  
	Fx_var_fixed =:= vl  andalso  erlang:size(Data) =< Flength andalso erlang:size(Data) > 0->
		perform_validation(Data,Data_type,Sub_format);

validate_data_sub({Data,Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format})when  
	Fx_var_fixed =:= fx  andalso  erlang:size(Data) =:= Flength andalso erlang:size(Data) > 0->
		perform_validation(Data,Data_type,Sub_format);


validate_data_sub(_)->
	{error,<<"Length Error">>}.	


%% @doc for performing validation on each data element  for a number
-spec perform_validation(binary(),list(),list())-> ok | {error,binary()}.
perform_validation(Data,"N","N")  ->
	case run_regex(Data,"^[0-9]*$",[]) of 
		true ->
			ok;
		false ->
			{error,<<"Format Error">>}
	end;


%% @doc for performing validation on each data element for a signed number beginning with C or D
perform_validation(Data,"N","SN")->
	 case run_regex(Data,"^[C|D][0-9]*$",[]) of
		true ->
			ok;
		false ->
			{error,<<"Format Error">>}
	end;		


%%for performinig validaton for a floating point numberic digit without  +\-
perform_validation(Data,"N","NF")->
	case run_regex(Data,"^[0-9]*\.?[0-9]+$",[]) of
		true ->
			ok;
		false ->
			{error,<<"Format Error">>}
	end;	

%%for performinig validaton for a signed floating point numeric digit number beginning wiht a credit/debit without +|-
perform_validation(Data,"N","SNF")->
	case run_regex(Data,"^[C|D][0-9]*\.?[0-9]+$",[]) of
		true ->
			ok;
		false ->
			{error,<<"Format Error">>}
	end;	


%%fr performing regex of alphabets
perform_validation(Data,"A","A")->
	case run_regex(Data,"^[a-zA-Z]*$",[]) of
		true ->
			ok;
		false ->
			{error,<<"Format Error">>}
	end;



%% for performing alpha numeric checks
perform_validation(Data,"AN","AN")->
	case run_regex(Data,"^[a-zA-Z0-9]*$",[]) of
		true ->
			ok;
		false ->
			{error,<<"Format Error">>}
	end;

%% for performing alpha numeric checks
perform_validation(Data,"ANS","ANS")->
	ok;


perform_validation(_Data,"S","S")->
	ok;


perform_validation(_Data,"NS","NS")->
	ok;


perform_validation(_Data,"B","B")->
	ok;


perform_validation(Data=[MM,DD,HH,MM,SS],"N","MMDDhhmmss")->
	ok;

perform_validation(Data,"N","hhmmss")->
	run_regex(Data,"",[]);


perform_validation(__Data,"N","YYMMddhhmmss")->
	ok;

perform_validation(_Data,"N","MMDD")->
	ok;

perform_validation(_Data,"N","YYMM")->
	ok;
	
perform_validation(_Data,"N","YYMMDD")->
	ok;



perform_validation(_,_,_)->
	{error,<<"Format Error Or Length Error">>}.


run_regex(Data,Regex_Expression,Options)->
  case re:run(Data, Regex_Expression,Options) of
      match -> true;
      {match, _} -> true;
      _ -> false
  end.
