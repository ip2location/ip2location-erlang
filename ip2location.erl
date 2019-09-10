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
	"8.1.0".

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

readcolcountry(S, Dbtype, Rowoffset, Col) ->
	X = "This parameter is unavailable for selected data file. Please upgrade the data file.",
	case lists:nth(Dbtype, Col) of
	0 ->
		{X, X};
	Colpos ->
		Coloffset = (Colpos - 1) bsl 2,
		X0 = readuint32(S, Rowoffset + Coloffset),
		X1 = readstr(S, X0),
		X2 = readstr(S, X0 + 3),
		{X1, X2}
	end.

readcolstring(S, Dbtype, Rowoffset, Col) ->
	case lists:nth(Dbtype, Col) of
	0 ->
		"This parameter is unavailable for selected data file. Please upgrade the data file.";
	Colpos ->
		Coloffset = (Colpos - 1) bsl 2,
		readstr(S, readuint32(S, Rowoffset + Coloffset))
	end.
	

readcolfloat(S, Dbtype, Rowoffset, Col) ->
	case lists:nth(Dbtype, Col) of
	0 ->
		0.0;
	Colpos ->
		Coloffset = (Colpos - 1) bsl 2,
		round(readfloat(S, Rowoffset + Coloffset), 6)
	end.

readcolfloatstring(S, Dbtype, Rowoffset, Col) ->
	case lists:nth(Dbtype, Col) of
	0 ->
		0.0;
	Colpos ->
		Coloffset = (Colpos - 1) bsl 2,
		N = readstr(S, readuint32(S, Rowoffset + Coloffset)),
		case string:to_float(N) of
		{error,no_float} ->
			list_to_integer(N);
		{F,_Rest} ->
			F
		end
	end.

readrecord(S, Dbtype, Rowoffset) ->
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
	
	{Country_short, Country_long} = readcolcountry(S, Dbtype, Rowoffset, Country_position),
	Region = readcolstring(S, Dbtype, Rowoffset, Region_position),
	City = readcolstring(S, Dbtype, Rowoffset, City_position),
	Isp = readcolstring(S, Dbtype, Rowoffset, Isp_position),
	Latitude = readcolfloat(S, Dbtype, Rowoffset, Latitude_position),
	Longitude = readcolfloat(S, Dbtype, Rowoffset, Longitude_position),
	Domain = readcolstring(S, Dbtype, Rowoffset, Domain_position),
	Zipcode = readcolstring(S, Dbtype, Rowoffset, Zipcode_position),
	Timezone = readcolstring(S, Dbtype, Rowoffset, Timezone_position),
	Netspeed = readcolstring(S, Dbtype, Rowoffset, Netspeed_position),
	Iddcode = readcolstring(S, Dbtype, Rowoffset, Iddcode_position),
	Areacode = readcolstring(S, Dbtype, Rowoffset, Areacode_position),
	Weatherstationcode = readcolstring(S, Dbtype, Rowoffset, Weatherstationcode_position),
	Weatherstationname = readcolstring(S, Dbtype, Rowoffset, Weatherstationname_position),
	Mcc = readcolstring(S, Dbtype, Rowoffset, Mcc_position),
	Mnc = readcolstring(S, Dbtype, Rowoffset, Mnc_position),
	Mobilebrand = readcolstring(S, Dbtype, Rowoffset, Mobilebrand_position),
	Elevation = readcolfloatstring(S, Dbtype, Rowoffset, Elevation_position),
	Usagetype = readcolstring(S, Dbtype, Rowoffset, Usagetype_position),
	
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
	Fromv4mapped = 281470681743360,
	Tov4mapped = 281474976710655,
	From6to4 = 42545680458834377588178886921629466624,
	To6to4 = 42550872755692912415807417417958686719,
	Fromteredo = 42540488161975842760550356425300246528,
	Toteredo = 42540488241204005274814694018844196863,
	Last32bits = 4294967295,
	
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
			
			Result = case inet:parse_address(Ip) of
			{ok, {X1, X2, X3, X4}} ->
				Ipnum = (X1 bsl 24) + (X2 bsl 16) + (X3 bsl 8) + (X4),
				search4(S, Ipnum, Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
			{ok, {X1, X2, X3, X4, X5, X6, X7, X8}} ->
				Ipnum = (X1 bsl 112) + (X2 bsl 96) + (X3 bsl 80) + (X4 bsl 64) + (X5 bsl 48) + (X6 bsl 32) + (X7 bsl 16) + X8,
				if
					Ipnum >= Fromv4mapped andalso Ipnum =< Tov4mapped ->
						search4(S, Ipnum - Fromv4mapped, Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
					Ipnum >= From6to4 andalso Ipnum =< To6to4 ->
						search4(S, (Ipnum bsr 80) band Last32bits, Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
					Ipnum >= Fromteredo andalso Ipnum =< Toteredo ->
						search4(S, ((bnot Ipnum) band Last32bits), Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
					true ->
						search6(S, Ipnum, Databasetype, 0, Ipv6databasecount, Ipv6databaseaddr, Ipv6indexbaseaddr, Ipv6columnsize)
				end;
			{_, _} ->
				io:format("Error: Invalid IP address.~n", []),
				{} % return empty
			end,
			file:close(S),
			Result
		end
	end.

close() ->
	case ets:info(mymeta) of
	undefined ->
		ok; % do nothing
	_ ->
		ets:delete(mymeta)
	end.
