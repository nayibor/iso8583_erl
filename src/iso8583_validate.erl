%%%
%%% @doc iso8583_ascii module.
%%%<br>this module is performing various validations</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_validate).

-export([validate_data/2]).


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
					Final_form = iso8583_ascii:get_data_element(Value_iso,Data_type),
					New_result_map = maps:put(Key_iso,Final_form,Map_result),
					{{ok,New_result_map},{error,Error_list}};
				{error,Result} ->
					{{ok,Map_result},{error,[{Key_iso,Result}|Error_list]}}
			end;
		(_,_,Acc_in)->
			Acc_in
		end,		
	maps:fold(Map_fold_iso_function,{ok,maps:new(),{error,[]}},Data_map).




validate_data_sub({Data,Data_type,Flength,Fx_var_fixed,Fx_header_length,Sub_format})->
	perform_validation(Data,Data_type,Sub_format,Flength).


%% @doc for performing validation on each data element 
-spec perform_validation(tuple(),list(),list(),non_neg_integer())-> ok | {error,binary()}.
perform_validation(Data,"N","N",Flength)->
	case run_regex(Data,"^[0-9]*$",[]) =:= ok andalso erlang:size(Data) =< Flength of 
		true ->
			ok;
		false ->
			{error,<<"Format Error">>}
	end;
perform_validation(Data,"N","SN",_Flength)->
	ok;
perform_validation(_Data,"N","NF",_Flength)->
	ok;
perform_validation(__Data,"N","MMDDhhmmss",_Flength)->
	ok;
perform_validation(_Data,"N","hhmmss",_Flength)->
	ok;
perform_validation(_Data,"N","MMDD",_Flength)->
	ok;
perform_validation(_Data,"N","YYMM",_Flength)->
	ok;
perform_validation(_Data,"N","YYMMDD",_Flength)->
	ok;
perform_validation(_Data,"NS","NS",_Flength)->
	ok;
perform_validation(_Data,"A","A",_Flength)->
	ok;
perform_validation(_Data,"ANS","ANS",_Flength)->
	ok;
perform_validation(_Data,"AN","AN",_Flength)->
	ok;
perform_validation(_Data,"S","S",_Flength)->
	ok;
perform_validation(_Data,"B","B",_Flength)->
	ok;
perform_validation(_Data,_,_,_)->
	{error,<<"unknown format">>}.


run_regex(Data,Regex_Expression,Options)->
  case re:run(Data, Regex_Expression,Options) of
      nomatch    -> ok;
      {match, _} -> error
  end.
