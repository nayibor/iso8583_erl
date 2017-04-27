%%%
%%% @doc yapp_test_ascii_def module.
%%%<br>this module contains specs for ascii</br>
%%% @end
%%% @copyright Nuku Ameyibor <nayibor@startmail.com>


-module(iso8583_ascii_def).


-export([get_spec_field/1]).




%% @doc this part gets the specifications for a particular field for ascii 1993 messages
-spec get_spec_field(pos_integer())->{atom(),pos_integer(),atom(),pos_integer(),binary()} .
get_spec_field(DataElem)->
		case DataElem of
			1	->{b,16,fx,0,<<"Secondary Bitmap">>};%%small change 
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
			12	->{n,12,fx,0,<<"Time, Local transaction (YYMMDDhhmmss)">>};
			13	->{n,4,fx,0,<<"Date, Local transaction (MMDD)">>};
			14	->{n,4,fx,0,<<"Date, Expiration YYMM">>};
			15	->{n,6,fx,0,<<"Settlement Date YYMMDD">>};
			16	->{n,4,fx,0,<<"Conversion Date MMDD">>};
			17	->{n,4,fx,0,<<"Date, Capture MMDD">>};
			18	->{n,4,fx,0,<<"Merchant Type">>};
			19	->{n,3,fx,0,<<"Country Code">>};
			20	->{n,3,fx,0,<<"Country Code/Pan Extended">>};
			21	->{n,3,fx,0,<<"Country Code/Forwarding Institution">>};
			22	->{an,12,fx,0,<<"Pos Data Code">>};
			23	->{n,3,fx,0,<<"Card Sequence Number">>};				
			24	->{n,3,fx,0,<<"Function Code">>};
			25	->{n,4,fx,0,<<"Message Reason Code">>};
			26	->{n,4,fx,0,<<"Card Acceptor Business Code">>};
			27	->{n,1,fx,0,<<"Approval Code">>};
			28	->{n,6,fx,0,<<"Reconciliation Date YYMMDD">>};
			29	->{n,3,fx,0,<<"Reconciliation Indicator">>};
			30	->{n,24,fx,0,<<"Amount Original">>};
			31	->{ans,99,vl,2,<<"Acquirer Reference Data">>};
			32	->{n,11,vl,2,<<"Acquirre Identification Code">>};
			33	->{n,11,vl,2,<<"Forwarding Identification Code">>};
			34	->{ns,28,vl,2,<<"Pan Extended">>};
			35	->{ns,37,vl,2,<<"Track 2 Data">>};
			36	->{ns,104,vl,2,<<"Track 3 Data">>};
			37	->{anp,12,fx,0,<<"Retrieval Reference Number">>};
			38	->{anp,6,fx,0,<<"Approval Code">>};
			39	->{n,3,fx,0,<<"Response Code">>};
			40	->{n,3,fx,0,<<"Service Code">>};
			41	->{ans,8,fx,0,<<"Terminal Id">>};
			42	->{ans,15,fx,0,<<"Card Acceptor Identication Code">>};
			43	->{ans,99,vl,2,<<"Name/Location">>};
			44	->{ans,99,vl,2,<<"Additional Response Data">>};
			45	->{ans,76,vl,2,<<"Track 1 Data">>};
			46	->{ans,204,vl,2,<<"Amount/Fees">>};
			47	->{ans,999,vl,3,<<"Additional Data National">>};%%small change
			48	->{ans,999,vl,3,<<"Additional Data Private">>};	%%small change
			49	->{aorn,3,fx,0,<<"Currency Code Transaction">>};
			50	->{aorn,3,fx,0,<<"Currency Code Reconciliaton">>};
			51	->{aorn,3,fx,0,<<"Currency Code Cardholder Billing">>};
			52	->{hex,8,fx,0,<<"Pin Data">>};
			53	->{b,48,vl,2,<<"Crypto Info">>};
			54	->{ans,120,vl,2,<<"aAmount Additional">>};
			55	->{b,255,vl,2,<<"Currency Code Cardholder Billing">>};
			56	->{n,35,vl,2,<<"Original Data Elements">>};
			57	->{n,3,fx,0,<<"Authorization Life Cycle Code">>};
			58	->{n,11,vl,2,<<"Authorization Agent Inst Id Code">>};
			59	->{ans,999,vl,2,<<"Transport Code">>};
			60	->{ans,999,vl,3,<<"Reserved For Nation Use">>};%%small change
			61	->{ans,999,vl,3,<<"Reserved For Nation Use">>};%%small change
			62	->{ans,999,vl,3,<<"Reserved For Nation Use">>};%%small change
			63	->{ans,999,vl,3,<<"Reserved For Nation Use">>};%%small change
			64	->{hex,8,fx,0,<<"Mac Data">>};
			65	->{t,8,fx,0,<<"Reserved for Iso Use">>};
			66	->{ans,204,vl,2,<<"Amount Original Fees">>};	
			67	->{n,2,fx,0,<<"Extended Payment Data">>};				
			68	->{n,3,fx,0,<<"Country Code,Receiving Institution">>};				
			69	->{n,3,fx,0,<<"Country Code,Settlement Institution">>};				
			70	->{n,3,fx,0,<<"Country Code,Authorizing Agent  Institution">>};				
			71	->{n,8,fx,0,<<"Message Number">>};				
			72	->{ans,255,vl,2,<<"Data Record">>};				
			73	->{n,6,fx,0,<<"Date Action YYMMDD">>};				
			74	->{n,10,fx,0,<<"Credits Number">>};
			75	->{n,10,fx,0,<<"Credits Reversal Number">>};				
			76	->{n,10,fx,0,<<"Debits Number">>};				
			77	->{n,10,fx,0,<<"Debits Reversal Number">>};				
			78	->{n,10,fx,0,<<"Transfer Number">>};				
			79	->{n,10,fx,0,<<"Transfer Reversal Number">>};				
			80	->{n,10,fx,0,<<"Enquiries Number">>};				
			81	->{n,10,fx,0,<<"Authorizations Number">>};				
			82	->{n,10,fx,0,<<"Enquiries Reversal Number">>};				
			83	->{n,10,fx,0,<<"Payments Number">>};				
			84	->{n,10,fx,0,<<"Payments Reversal Number">>};				
			85	->{n,10,fx,0,<<"Fee Collection Number">>};				
			86	->{n,16,fx,0,<<"Credits Amount">>};				
			87	->{n,16,fx,0,<<"Credits Reversal Amount">>};				
			88	->{n,16,fx,0,<<"Debits Amount">>};				
			89	->{n,16,fx,0,<<"Debits Reversal Amount">>};				
			90	->{n,10,fx,0,<<"Authrization Reversal Number">>};				
			91	->{n,3,fx,0,<<"Country Code.Transaction Destination Institution">>};				
			92	->{n,3,fx,0,<<"Country Code.Transaction Originator Institution">>};				
			93	->{n,11,vl,2,<<"Transaction Destination Institution Id Code">>};				
			94	->{n,11,vl,2,<<"Transaction Originator Institution Id Code">>};				
			95	->{ans,99,vl,2,<<"Transaction Originator Institution Id Code">>};				
			96	->{b,255,vl,2,<<"Key Management Data">>};				
			97	->{n,16,fx,0,<<"Amount Net Reconciliation">>};				
			98	->{ans,25,fx,0,<<"Third Party Information">>};				
			99	->{an,11,vl,2,<<"Settlement Instituition Id">>};				
			100	->{n,11,vl,2,<<"Receiving Instituition Id">>};				
			101	->{ans,17,vl,2,<<"File Name">>};				
			102	->{ans,28,vl,2,<<"Account Number">>};				
			103	->{ans,28,vl,2,<<"Account Number 2">>};				
			104	->{ans,100,vl,2,<<"Transaction Description">>};				
			105	->{n,16,fx,0,<<"Credits ChargeBack Amount">>};				
			106	->{n,16,fx,0,<<"Debits ChargeBack Amount">>};				
			107	->{n,10,fx,0,<<"Credits Chargeback Number">>};				
			108	->{n,10,fx,0,<<"Debits Chargeback Number">>};				
			109	->{ans,84,vl,2,<<"Credits Fee Amount">>};				
			110	->{ans,84,vl,2,<<"Debits Fee Amount">>};				
			111	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			112	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			113	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			114	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			115	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			116	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			117	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			118	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			119	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			120	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			121	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			122	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			123	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			124	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			125	->{ans,255,vl,3,<<"Reserved For Iso Use">>};				
			126	->{ans,255,vl,3,<<"Reserved For Iso Use">>}				
		end .


