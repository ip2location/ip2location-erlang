-module(ip2location).
-export([apiversion/0, getapiversion/0, new/1, query/1, close/0]).
-record(ip2locationrecord, {
	country_short = "-",
	country_long = "-",
	region = "-",
	city = "-",
	isp = "-",
	latitude = 0,
	longitude = 0,
	domain = "-",
	zipcode = "-",
	timezone = "-",
	netspeed = "-",
	iddcode = "-",
	areacode = "-",
	weatherstationcode = "-",
	weatherstationname = "-",
	mcc = "-",
	mnc = "-",
	mobilebrand = "-",
	elevation = 0,
	usagetype = "-"
}).

apiversion() ->
	"8.0.3".

getapiversion() ->
	io:format("API Version: ~p~n", [apiversion()]).

round(Number, Precision) ->
	P = math:pow(10, Precision),
	round(Number * P) / P.

readuint(S, StartPos, Len) ->
	case file:pread(S, StartPos - 1, Len) of
	eof ->
		ok;
	{ok, Data} ->
		binary:decode_unsigned(Data, little)
	end.

readuint8(S, StartPos) ->
	readuint(S, StartPos, 1).

readuint32(S, StartPos) ->
	readuint(S, StartPos, 4).

readuint128(S, StartPos) ->
	readuint(S, StartPos, 16).

readstr(S, StartPos) ->
	case file:pread(S, StartPos, 1) of
	eof ->
		ok;
	{ok, LenRaw} ->
		Len = binary:decode_unsigned(LenRaw, little),
		case file:pread(S, StartPos + 1, Len) of
		eof ->
			ok;
		{ok, Data} ->
			binary_to_list(Data)
		end
	end.

readfloat(S, StartPos) ->
	case file:pread(S, StartPos - 1, 4) of
	eof ->
		ok;
	{ok, Data} ->
		<<F:32/float-little>> = Data,
		F
	end.

input(InputFile) ->
	case file:open(InputFile, [read, binary, raw]) of
	{ok, S} ->
		S;
	{_, _} ->
		io:format("Error: Invalid BIN file.~n", []),
		halt()
	end.

new(InputFile) ->
	S = input(InputFile),
	Databasetype = readuint8(S, 1),
	Databasecolumn = readuint8(S, 2),
	% Databaseyear = readuint8(S, 3),
	% Databasemonth = readuint8(S, 4),
	% Databaseday = readuint8(S, 5),
	Ipv4databasecount = readuint32(S, 6),
	Ipv4databaseaddr = readuint32(S, 10),
	Ipv6databasecount = readuint32(S, 14),
	Ipv6databaseaddr = readuint32(S, 18),
	Ipv4indexbaseaddr = readuint32(S, 22),
	Ipv6indexbaseaddr = readuint32(S, 26),
	Ipv4columnsize = Databasecolumn bsl 2, % 4 bytes each column
	Ipv6columnsize = 16 + ((Databasecolumn - 1) bsl 2), % 4 bytes each column, except IPFrom column which is 16 bytes
	file:close(S),
	
	case ets:info(mymeta) of
	undefined ->
		ets:new(mymeta, [set, named_table]);
	_ ->
		ok % do nothing
	end,
	
	ets:insert(mymeta, {inputfile, InputFile}),
	ets:insert(mymeta, {databasetype, Databasetype}),
	ets:insert(mymeta, {databasetype, Databasetype}),
	ets:insert(mymeta, {databasecolumn, Databasecolumn}),
	% ets:insert(mymeta, {databaseyear, Databaseyear}),
	% ets:insert(mymeta, {databasemonth, Databasemonth}),
	% ets:insert(mymeta, {databaseday, Databaseday}),
	ets:insert(mymeta, {ipv4databasecount, Ipv4databasecount}),
	ets:insert(mymeta, {ipv4databaseaddr, Ipv4databaseaddr}),
	ets:insert(mymeta, {ipv6databasecount, Ipv6databasecount}),
	ets:insert(mymeta, {ipv6databaseaddr, Ipv6databaseaddr}),
	ets:insert(mymeta, {ipv4indexbaseaddr, Ipv4indexbaseaddr}),
	ets:insert(mymeta, {ipv6indexbaseaddr, Ipv6indexbaseaddr}),
	ets:insert(mymeta, {ipv4columnsize, Ipv4columnsize}),
	ets:insert(mymeta, {ipv6columnsize, Ipv6columnsize}).

readrecord(S, Dbtype, Rowoffset) ->
	Not_supported = "This parameter is unavailable for selected data file. Please upgrade the data file.",
	Country_position = [0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2],
	Region_position = [0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3],
	City_position = [0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4],
	Isp_position = [0, 0, 3, 0, 5, 0, 7, 5, 7, 0, 8, 0, 9, 0, 9, 0, 9, 0, 9, 7, 9, 0, 9, 7, 9],
	Latitude_position = [0, 0, 0, 0, 0, 5, 5, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5],
	Longitude_position = [0, 0, 0, 0, 0, 6, 6, 0, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6],
	Domain_position = [0, 0, 0, 0, 0, 0, 0, 6, 8, 0, 9, 0, 10,0, 10, 0, 10, 0, 10, 8, 10, 0, 10, 8, 10],
	Zipcode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 7, 7, 7, 0, 7, 7, 7, 0, 7, 0, 7, 7, 7, 0, 7],
	Timezone_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 7, 8, 8, 8, 7, 8, 0, 8, 8, 8, 0, 8],
	Netspeed_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 11,0, 11,8, 11, 0, 11, 0, 11, 0, 11],
	Iddcode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 12, 0, 12, 0, 12, 9, 12, 0, 12],
	Areacode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10 ,13 ,0, 13, 0, 13, 10, 13, 0, 13],
	Weatherstationcode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 14, 0, 14, 0, 14, 0, 14],
	Weatherstationname_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 15, 0, 15, 0, 15, 0, 15],
	Mcc_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 16, 0, 16, 9, 16],
	Mnc_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10,17, 0, 17, 10, 17],
	Mobilebrand_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11,18, 0, 18, 11, 18],
	Elevation_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 19, 0, 19],
	Usagetype_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 20],
	
	case lists:nth(Dbtype, Country_position) of
		0 ->
			Country_short = Not_supported,
			Country_long = Not_supported;
		Colpos ->
			Coloffset = (Colpos - 1) bsl 2,
			Country_short = readstr(S, readuint32(S, Rowoffset + Coloffset)),
			Country_long = readstr(S, readuint32(S, Rowoffset + Coloffset) + 3)
	end,
	
	case lists:nth(Dbtype, Region_position) of
		0 ->
			Region = Not_supported;
		Colpos2 ->
			Coloffset2 = (Colpos2 - 1) bsl 2,
			Region = readstr(S, readuint32(S, Rowoffset + Coloffset2))
	end,
	
	case lists:nth(Dbtype, City_position) of
		0 ->
			City = Not_supported;
		Colpos3 ->
			Coloffset3 = (Colpos3 - 1) bsl 2,
			City = readstr(S, readuint32(S, Rowoffset + Coloffset3))
	end,
	
	case lists:nth(Dbtype, Isp_position) of
		0 ->
			Isp = Not_supported;
		Colpos4 ->
			Coloffset4 = (Colpos4 - 1) bsl 2,
			Isp = readstr(S, readuint32(S, Rowoffset + Coloffset4))
	end,
	
	case lists:nth(Dbtype, Latitude_position) of
		0 ->
			Latitude = 0.0;
		Colpos5 ->
			Coloffset5 = (Colpos5 - 1) bsl 2,
			Latitude = round(readfloat(S, Rowoffset + Coloffset5), 6)
	end,
	
	case lists:nth(Dbtype, Longitude_position) of
		0 ->
			Longitude = 0.0;
		Colpos6 ->
			Coloffset6 = (Colpos6 - 1) bsl 2,
			Longitude = round(readfloat(S, Rowoffset + Coloffset6), 6)
	end,
	
	case lists:nth(Dbtype, Domain_position) of
		0 ->
			Domain = Not_supported;
		Colpos7 ->
			Coloffset7 = (Colpos7 - 1) bsl 2,
			Domain = readstr(S, readuint32(S, Rowoffset + Coloffset7))
	end,
	
	case lists:nth(Dbtype, Zipcode_position) of
		0 ->
			Zipcode = Not_supported;
		Colpos8 ->
			Coloffset8 = (Colpos8 - 1) bsl 2,
			Zipcode = readstr(S, readuint32(S, Rowoffset + Coloffset8))
	end,
	
	case lists:nth(Dbtype, Timezone_position) of
		0 ->
			Timezone = Not_supported;
		Colpos9 ->
			Coloffset9 = (Colpos9 - 1) bsl 2,
			Timezone = readstr(S, readuint32(S, Rowoffset + Coloffset9))
	end,
	
	case lists:nth(Dbtype, Netspeed_position) of
		0 ->
			Netspeed = Not_supported;
		Colpos10 ->
			Coloffset10 = (Colpos10 - 1) bsl 2,
			Netspeed = readstr(S, readuint32(S, Rowoffset + Coloffset10))
	end,
	
	case lists:nth(Dbtype, Iddcode_position) of
		0 ->
			Iddcode = Not_supported;
		Colpos11 ->
			Coloffset11 = (Colpos11 - 1) bsl 2,
			Iddcode = readstr(S, readuint32(S, Rowoffset + Coloffset11))
	end,
	
	case lists:nth(Dbtype, Areacode_position) of
		0 ->
			Areacode = Not_supported;
		Colpos12 ->
			Coloffset12 = (Colpos12 - 1) bsl 2,
			Areacode = readstr(S, readuint32(S, Rowoffset + Coloffset12))
	end,
	
	case lists:nth(Dbtype, Weatherstationcode_position) of
		0 ->
			Weatherstationcode = Not_supported;
		Colpos13 ->
			Coloffset13 = (Colpos13 - 1) bsl 2,
			Weatherstationcode = readstr(S, readuint32(S, Rowoffset + Coloffset13))
	end,
	
	case lists:nth(Dbtype, Weatherstationname_position) of
		0 ->
			Weatherstationname = Not_supported;
		Colpos14 ->
			Coloffset14 = (Colpos14 - 1) bsl 2,
			Weatherstationname = readstr(S, readuint32(S, Rowoffset + Coloffset14))
	end,
	
	case lists:nth(Dbtype, Mcc_position) of
		0 ->
			Mcc = Not_supported;
		Colpos15 ->
			Coloffset15 = (Colpos15 - 1) bsl 2,
			Mcc = readstr(S, readuint32(S, Rowoffset + Coloffset15))
	end,
	
	case lists:nth(Dbtype, Mnc_position) of
		0 ->
			Mnc = Not_supported;
		Colpos16 ->
			Coloffset16 = (Colpos16 - 1) bsl 2,
			Mnc = readstr(S, readuint32(S, Rowoffset + Coloffset16))
	end,
	
	case lists:nth(Dbtype, Mobilebrand_position) of
		0 ->
			Mobilebrand = Not_supported;
		Colpos17 ->
			Coloffset17 = (Colpos17 - 1) bsl 2,
			Mobilebrand = readstr(S, readuint32(S, Rowoffset + Coloffset17))
	end,
	
	case lists:nth(Dbtype, Elevation_position) of
		0 ->
			Elevation = Not_supported;
		Colpos18 ->
			Coloffset18 = (Colpos18 - 1) bsl 2,
			N = readstr(S, readuint32(S, Rowoffset + Coloffset18)),
			case string:to_float(N) of
				{error,no_float} ->
					Elevation = list_to_integer(N);
				{F,_Rest} ->
					Elevation = F
			end
	end,
	
	case lists:nth(Dbtype, Usagetype_position) of
		0 ->
			Usagetype = Not_supported;
		Colpos19 ->
			Coloffset19 = (Colpos19 - 1) bsl 2,
			Usagetype = readstr(S, readuint32(S, Rowoffset + Coloffset19))
	end,
	
	#ip2locationrecord{
	country_short = Country_short,
	country_long = Country_long,
	region = Region,
	city = City,
	isp = Isp,
	latitude = Latitude,
	longitude = Longitude,
	domain = Domain,
	zipcode = Zipcode,
	timezone = Timezone,
	netspeed = Netspeed,
	iddcode = Iddcode,
	areacode = Areacode,
	weatherstationcode = Weatherstationcode,
	weatherstationname = Weatherstationname,
	mcc = Mcc,
	mnc = Mnc,
	mobilebrand = Mobilebrand,
	elevation = Elevation,
	usagetype = Usagetype
	}.

searchtree(S, Ipnum, Dbtype, Low, High, BaseAddr, Colsize, Iptype) ->
	if
		Low =< High ->
			Mid = ((Low + High) bsr 1),
			Rowoffset = BaseAddr + (Mid * Colsize),
			Rowoffset2 = Rowoffset + Colsize,
			
			if
				Iptype == ipv4 ->
					Ipfrom = readuint32(S, Rowoffset),
					Ipto = readuint32(S, Rowoffset2);
				true ->
					Ipfrom = readuint128(S, Rowoffset),
					Ipto = readuint128(S, Rowoffset2)
			end,
			
			if
				Ipnum >= Ipfrom andalso Ipnum < Ipto ->
					if
						Iptype == ipv4 ->
							readrecord(S, Dbtype + 1, Rowoffset);
						true ->
							readrecord(S, Dbtype + 1, Rowoffset + 12)
					end;
				true ->
					if
						Ipnum < Ipfrom ->
							searchtree(S, Ipnum, Dbtype, Low, Mid - 1, BaseAddr, Colsize, Iptype);
						true ->
							searchtree(S, Ipnum, Dbtype, Mid + 1, High, BaseAddr, Colsize, Iptype)
					end
			end;
		true ->
			io:format("Error: IP address not found.~n", []),
			{} % return empty
	end.

search4(S, Ipnum, Dbtype, Low, High, Baseaddr, Indexbaseaddr, Colsize) ->
	if
		Indexbaseaddr > 0 ->
			Indexpos = ((Ipnum bsr 16) bsl 3) + Indexbaseaddr,
			Low2 = readuint32(S, Indexpos),
			High2 = readuint32(S, Indexpos + 4),
			searchtree(S, Ipnum, Dbtype, Low2, High2, Baseaddr, Colsize, ipv4);
		true ->
			searchtree(S, Ipnum, Dbtype, Low, High, Baseaddr, Colsize, ipv4)
	end.

search6(S, Ipnum, Dbtype, Low, High, Baseaddr, Indexbaseaddr, Colsize) ->
	if
		Indexbaseaddr > 0 ->
			Indexpos = ((Ipnum bsr 112) bsl 3) + Indexbaseaddr,
			Low2 = readuint32(S, Indexpos),
			High2 = readuint32(S, Indexpos + 4),
			searchtree(S, Ipnum, Dbtype, Low2, High2, Baseaddr, Colsize, ipv6);
		true ->
			searchtree(S, Ipnum, Dbtype, Low, High, Baseaddr, Colsize, ipv6)
	end.

query(Ip) ->
	From = 281470681743360,
	To = 281474976710655,
	
	case ets:info(mymeta) of
	undefined ->
		io:format("Error: Unable to read metadata.~n", []),
		{}; % return empty
	_ ->
		case ets:lookup(mymeta, inputfile) of
		[] ->
			io:format("Error: Unable to read metadata.~n", []),
			{}; % return empty
		[{_, InputFile}] ->
			S = input(InputFile),
			[{_, Databasetype}] = ets:lookup(mymeta, databasetype),
			[{_, Databasetype}] = ets:lookup(mymeta, databasetype),
			% [{_, Databasecolumn}] = ets:lookup(mymeta, databasecolumn),
			% [{_, Databaseyear}] = ets:lookup(mymeta, databaseyear),
			% [{_, Databasemonth}] = ets:lookup(mymeta, databasemonth),
			% [{_, Databaseday}] = ets:lookup(mymeta, databaseday),
			[{_, Ipv4databasecount}] = ets:lookup(mymeta, ipv4databasecount),
			[{_, Ipv4databaseaddr}] = ets:lookup(mymeta, ipv4databaseaddr),
			[{_, Ipv6databasecount}] = ets:lookup(mymeta, ipv6databasecount),
			[{_, Ipv6databaseaddr}] = ets:lookup(mymeta, ipv6databaseaddr),
			[{_, Ipv4indexbaseaddr}] = ets:lookup(mymeta, ipv4indexbaseaddr),
			[{_, Ipv6indexbaseaddr}] = ets:lookup(mymeta, ipv6indexbaseaddr),
			[{_, Ipv4columnsize}] = ets:lookup(mymeta, ipv4columnsize),
			[{_, Ipv6columnsize}] = ets:lookup(mymeta, ipv6columnsize),
			
			case inet:parse_address(Ip) of
			{ok, {X1, X2, X3, X4}} ->
				Ipnum = (X1 bsl 24) + (X2 bsl 16) + (X3 bsl 8) + (X4),
				search4(S, Ipnum, Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
			{ok, {X1, X2, X3, X4, X5, X6, X7, X8}} ->
				Ipnum = (X1 bsl 112) + (X2 bsl 96) + (X3 bsl 80) + (X4 bsl 64) + (X5 bsl 48) + (X6 bsl 32) + (X7 bsl 16) + X8,
				if
					Ipnum >= From andalso Ipnum =< To ->
						search4(S, Ipnum - To, Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
					true ->
						search6(S, Ipnum, Databasetype, 0, Ipv6databasecount, Ipv6databaseaddr, Ipv6indexbaseaddr, Ipv6columnsize)
				end;
			{_, _} ->
				io:format("Error: Invalid IP address.~n", []),
				{} % return empty
			end
		end
	end.

close() ->
	case ets:info(mymeta) of
	undefined ->
		ok; % do nothing
	_ ->
		ets:delete(mymeta)
	end.
