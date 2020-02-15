%%%
%%% @doc iso8583_ascii_post module.
%%%<br>this module contains specs for postillion</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_ascii_def).

%%for exporting functions for getting the various fields
-export([get_spec_field/1,get_bitmap_type/0]).


%% @doc this part gets the specifications for a particular field  for postillion messages
-spec get_spec_field(pos_integer())-> 
		{
			Fieldtype::atom(),
			Fieldlength::pos_integer(),
			Fieldflex::atom(),
			Headerlength::pos_integer(),
			FieldName::binary()
		} .


-spec get_bitmap_type()->binary|hex.	
get_bitmap_type()->
		hex.


get_spec_field(DataElem)->
		case DataElem of
			1	->{n,4,fx,0,<<"Mti">>};
			2 	->{n,19,vl,2,<<"Pan">>};
			3 	->{n,6,fx,0,<<"Processing Code">>};
			4 	->{n,12,fx,0,<<"Amount Transaction">>};
			5 	->{n,12,fx,0,<<"Amount Settlement">>};
			6 	->{n,12,fx,0,<<"Amount, cardholder billing">>};
			7 	->{n,10,fx,0,<<"Transmission date & time MMDDhhmmss">>};
			8 	->{n,8,fx,0,<<"Amount, Cardholder billing fee">>};
			9 	->{n,8,fx,0,<<"Conversion rate, Settlement">>};
			10	->{n,8,fx,0,<<"Conversion rate, cardholder billing">>};
			11	->{n,6,fx,0,<<"System Trace Audit Number">>};
			12	->{n,6,fx,0,<<"Time, Local transaction (hhmmss)">>};
			13	->{n,4,fx,0,<<"Date, Local transaction (MMDD)">>};
			14	->{n,4,fx,0,<<"Date, Expiration YYMM">>};
			15	->{n,4,fx,0,<<"Settlement Date MMDD">>};
			16	->{n,4,fx,0,<<"Conversion Date MMDD">>};
			17	->{n,4,fx,0,<<"Date, Capture MMDD">>};
			18	->{n,4,fx,0,<<"Merchant Type">>};
			19	->{n,3,fx,0,<<"Country Code">>};
			20	->{n,3,fx,0,<<"Country Code/Pan Extended">>};
			21	->{n,3,fx,0,<<"Country Code/Forwarding Institution">>};
			22	->{n,3,fx,0,<<"Pos Data Code">>};
			23	->{n,3,fx,0,<<"Card Sequence Number">>};				
			24	->{n,3,fx,0,<<"Network International Identifier">>};
			25	->{n,2,fx,0,<<"Message Reason Code">>};
			26	->{n,2,fx,0,<<"POINT OF SERVICE PIN CAPTURE CODE">>};
			27	->{n,1,fx,0,<<"AUTHORIZATION IDENTIFICATION RESP LEN">>};
			28	->{n,9,fx,0,<<"AMOUNT, TRANSACTION FEE">>};
			29	->{n,9,fx,0,<<"AMOUNT, SETTLEMENT FEE">>};
			30	->{n,9,fx,0,<<"AMOUNT, TRANSACTION PROCESSING FEE">>};
			31	->{n,9,fx,2,<<"AMOUNT, SETTLEMENT PROCESSING FEE">>};
			32	->{n,11,vl,2,<<"ACQUIRING INSTITUTION IDENT CODE">>};
			33	->{n,11,vl,2,<<"FORWARDING INSTITUTION IDENT CODE">>};
			34	->{ns,28,vl,2,<<"Pan Extended">>};
			35	->{ns,37,vl,2,<<"Track 2 Data">>};
			36	->{ns,104,vl,3,<<"Track 3 Data">>};
			37	->{ans,12,fx,0,<<"Retrieval Reference Number">>};
			38	->{ans,6,fx,0,<<"Approval Code">>};
			39	->{ans,2,fx,0,<<"Response Code">>};
			40	->{n,3,fx,0,<<"Service Code">>};
			41	->{ans,8,fx,0,<<"Terminal Id">>};
			42	->{ans,15,fx,0,<<"Card Acceptor Identication Code">>};
			43	->{b,40,fx,0,<<"Name/Location">>};%%check be careful
			44	->{ans,25,vl,2,<<"Additional Response Data">>};
			45	->{ans,76,vl,2,<<"Track 1 Data">>};
			46	->{ans,999,vl,3,<<"ADITIONAL DATA - ISO">>};
			47	->{ans,999,vl,3,<<"Additional Data National">>};
			48	->{ans,999,vl,3,<<"Additional Data Private">>};	
			49	->{aon,3,fx,0,<<"Currency Code Transaction">>};
			50	->{aon,3,fx,0,<<"Currency Code Reconciliaton">>};
			51	->{aon,3,fx,0,<<"Currency Code Cardholder Billing">>};
			52	->{b,8,fx,0,<<"Pin Data">>};%%check be careful
			53	->{b,48,vl,2,<<"Crypto Info">>};%%check be careful
			54	->{ans,120,vl,3,<<"aAmount Additional">>};
			55	->{b,999,vl,3,<<"Reserved For Nation Use">>};
			56	->{n,999,vl,3,<<"Reserved For Nation Use">>};
			57	->{n,999,vl,3,<<"Reserved For Nation Use">>};
			58	->{n,999,vl,3,<<"Reserved For Nation Use">>};
			59	->{ans,999,vl,3,<<"Reserved For Nation Use">>};
			60	->{ans,999,vl,3,<<"Reserved For Nation Use">>};
			61	->{ans,999,vl,3,<<"Reserved For Nation Use">>};
			62	->{ans,999,vl,3,<<"Reserved For Nation Use">>};
			63	->{ans,999,vl,3,<<"Reserved For Nation Use">>};
			64	->{hex,8,fx,0,<<"Mac Data">>};
			65	->{hex,8,fx,0,<<"Reserved for Iso Use">>};
			66	->{ans,1,fx,0,<<"Amount Original Fees">>};	
			67	->{n,2,fx,0,<<"Extended Payment Data">>};				
			68	->{n,3,fx,0,<<"Country Code,Receiving Institution">>};				
			69	->{n,3,fx,0,<<"Country Code,Settlement Institution">>};				
			70	->{n,3,fx,0,<<"NETWORK MANAGEMENT INFORMATION CODE">>};				
			71	->{n,4,fx,0,<<"Message Number">>};				
			72	->{n,4,fx,0,<<"MESSAGE NUMBER LAST">>};				
			73	->{n,6,fx,0,<<"Date Action YYMMDD">>};				
			74	->{n,10,fx,0,<<"Credits Number">>};
			75	->{n,10,fx,0,<<"Credits Reversal Number">>};				
			76	->{n,10,fx,0,<<"Debits Number">>};				
			77	->{n,10,fx,0,<<"Debits Reversal Number">>};				
			78	->{n,10,fx,0,<<"Transfer Number">>};				
			79	->{n,10,fx,0,<<"Transfer Reversal Number">>};				
			80	->{n,10,fx,0,<<"Enquiries Number">>};				
			81	->{n,10,fx,0,<<"Authorizations Number">>};				
			82	->{n,12,fx,0,<<"CREDITS, PROCESSING FEE AMOUNT">>};				
			83	->{n,12,fx,0,<<"CREDITS, TRANSACTION FEE AMOUNT">>};				
			84	->{n,12,fx,0,<<"DEBITS, PROCESSING FEE AMOUNT">>};				
			85	->{n,12,fx,0,<<"DEBITS, TRANSACTION FEE AMOUNT">>};				
			86	->{n,16,fx,0,<<"Credits Amount">>};				
			87	->{n,16,fx,0,<<"Credits Reversal Amount">>};				
			88	->{n,16,fx,0,<<"Debits Amount">>};				
			89	->{n,16,fx,0,<<"Debits Reversal Amount">>};				
			90	->{n,42,fx,0,<<"ORIGINAL DATA ELEMENTS">>};				
			91	->{n,1,fx,0,<<"FILE UPDATE CODE">>};				
			92	->{n,2,fx,0,<<"FILE SECURITY CODE">>};				
			93	->{a,5,fx,0,<<"RESPONSE INDICATOR">>};				
			94	->{a,7,fx,0,<<"SERVICE INDICATOR">>};				
			95	->{ans,42,fx,0,<<"REPLACEMENT AMOUNTS">>};				
			96	->{hex,8,fx,0,<<"MESSAGE SECURITY CODE">>};				
			97	->{n,17,fx,0,<<"AMOUNT, NET SETTLEMENT">>};				
			98	->{ans,25,fx,0,<<"PAYEE">>};				
			99	->{ans,11,vl,2,<<"SETTLEMENT INSTITUTION IDENT CODE">>};				
			100	->{ans,11,vl,2,<<"RECEIVING INSTITUTION IDENT CODE">>};				
			101	->{ans,17,vl,2,<<"File Name">>};				
			102	->{ans,28,vl,2,<<"Account Number">>};				
			103	->{ans,28,vl,2,<<"Account Number 2">>};				
			104	->{ans,100,vl,3,<<"Transaction Description">>};				
			105	->{ans,999,vl,3,<<"RESERVED ISO USE">>};				
			106	->{ans,999,vl,3,<<"RESERVED ISO USE">>};				
			107	->{ans,999,vl,3,<<"RESERVED ISO USE">>};				
			108	->{ans,999,vl,3,<<"RESERVED ISO USE">>};				
			109	->{ans,999,vl,3,<<"RESERVED ISO USE">>};				
			110	->{ans,999,vl,3,<<"RESERVED ISO USE">>};				
			111	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			112	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			113	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			114	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			115	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			116	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			117	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			118	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			119	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			120	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			121	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			122	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			123	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			124	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			125	->{ans,999,vl,3,<<"Reserved For Iso Use">>};				
			126	->{ans,999,vl,3,<<"Reserved For Iso Use">>};
			127	->{b,999999,vl,6,<<"Reserved For Iso Use">>};%%check be careful
			128	->{ans,999,vl,3,<<"MAC 2">>}
		end .


%%have to add a function here which returns the various messages and the optional/mandatory fields for each message for this mti type
%% it will return a list of fields which were not set up well


%%have to add a function here also which will work on subfields
%%will have to show you the spec for the various subfields for a specific field number which will aid in their processing


%%have to add function which validates data being sent,received

