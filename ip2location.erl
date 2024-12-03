-module(ip2location).
-export([apiversion/0, getapiversion/0, new/1, query/1, close/0, openws/3, lookup/3, getcredit/0]).
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
	usagetype = "-",
	addresstype = "-",
	category = "-",
	district = "-",
	asn = "-",
	as = "-"
}).
-define(IF(Cond), (case (Cond) of true -> (0); false -> (1) end)).

apiversion() ->
	"8.6.2".

getapiversion() ->
	io:format("API Version: ~p~n", [apiversion()]).

round(Number, Precision) ->
	P = math:pow(10, Precision),
	round(Number * P) / P.

readuintrow(R, StartPos, Len) ->
	Data = binary:part(R, StartPos, Len),
	binary:decode_unsigned(Data, little).

readuint8row(R, StartPos) ->
	readuintrow(R, StartPos, 1).

readuint32row(R, StartPos) ->
	readuintrow(R, StartPos, 4).

readuint128row(R, StartPos) ->
	readuintrow(R, StartPos, 16).

readstr(S, StartPos) ->
	case file:pread(S, StartPos, 256) of % max size of string field + 1 byte for the length
	eof ->
		ok;
	{ok, R} ->
		Len = readuint8row(R, 0),
		Data = binary:part(R, 1, Len),
		binary_to_list(Data)
	end.

readfloatrow(R, StartPos) ->
	Data = binary:part(R, StartPos, 4),
	<<F:32/float-little>> = Data,
	F.

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
	case file:pread(S, 0, 64) of % 64-byte header
	eof ->
		halt();
	{ok, Data} ->
		R = Data,
		Databasetype = readuint8row(R, 0),
		Databasecolumn = readuint8row(R, 1),
		Databaseyear = readuint8row(R, 2),
		% Databasemonth = readuint8row(R, 3),
		% Databaseday = readuint8row(R, 4),
		Ipv4databasecount = readuint32row(R, 5),
		Ipv4databaseaddr = readuint32row(R, 9),
		Ipv6databasecount = readuint32row(R, 13),
		Ipv6databaseaddr = readuint32row(R, 17),
		Ipv4indexbaseaddr = readuint32row(R, 21),
		Ipv6indexbaseaddr = readuint32row(R, 25),
		Productcode = readuint8row(R, 29),
		Ipv4columnsize = Databasecolumn bsl 2, % 4 bytes each column
		Ipv6columnsize = 16 + ((Databasecolumn - 1) bsl 2), % 4 bytes each column, except IPFrom column which is 16 bytes
		% Producttype = readuint8row(R, 30),
		% Filesize = readuint32row(R, 31),

		if
			% check if is correct BIN (should be 1 for IP2Location BIN file), also checking for zipped file (PK being the first 2 chars)
			(Productcode /= 1 andalso Databaseyear >= 21) orelse (Databasetype == 80 andalso Databasecolumn == 75) ->
				io:format("Incorrect IP2Location BIN file format. Please make sure that you are using the latest IP2Location BIN file.~n", []),
				halt();
			true ->
				case ets:info(mymeta) of
					undefined ->
						ets:new(mymeta, [set, named_table]),
						ets:insert(mymeta, {inputfile, InputFile}),
						ets:insert(mymeta, {databasetype, Databasetype}),
						ets:insert(mymeta, {databasetype, Databasetype}),
						ets:insert(mymeta, {databasecolumn, Databasecolumn}),
						ets:insert(mymeta, {ipv4databasecount, Ipv4databasecount}),
						ets:insert(mymeta, {ipv4databaseaddr, Ipv4databaseaddr}),
						ets:insert(mymeta, {ipv6databasecount, Ipv6databasecount}),
						ets:insert(mymeta, {ipv6databaseaddr, Ipv6databaseaddr}),
						ets:insert(mymeta, {ipv4indexbaseaddr, Ipv4indexbaseaddr}),
						ets:insert(mymeta, {ipv6indexbaseaddr, Ipv6indexbaseaddr}),
						ets:insert(mymeta, {ipv4columnsize, Ipv4columnsize}),
						ets:insert(mymeta, {ipv6columnsize, Ipv6columnsize});
					_ ->
						ok % do nothing
				end
		end
	end,
	file:close(S).
	
readcolcountryrow(S, R, Dbtype, Col) ->
	X = "This parameter is unavailable for selected data file. Please upgrade the data file.",
	case lists:nth(Dbtype, Col) of
	0 ->
		{X, X};
	Colpos ->
		Coloffset = (Colpos - 2) bsl 2,
		X0 = readuint32row(R, Coloffset),
		X1 = readstr(S, X0),
		X2 = readstr(S, X0 + 3),
		{X1, X2}
	end.

readcolstringrow(S, R, Dbtype, Col) ->
	case lists:nth(Dbtype, Col) of
	0 ->
		"This parameter is unavailable for selected data file. Please upgrade the data file.";
	Colpos ->
		Coloffset = (Colpos - 2) bsl 2,
		readstr(S, readuint32row(R, Coloffset))
	end.

readcolfloatrow(R, Dbtype, Col) ->
	case lists:nth(Dbtype, Col) of
	0 ->
		0.0;
	Colpos ->
		Coloffset = (Colpos - 2) bsl 2,
		round(readfloatrow(R, Coloffset), 6)
	end.

readcolfloatstringrow(S, R, Dbtype, Col) ->
	case lists:nth(Dbtype, Col) of
	0 ->
		0.0;
	Colpos ->
		Coloffset = (Colpos - 2) bsl 2,
		N = readstr(S, readuint32row(R, Coloffset)),
		case string:to_float(N) of
		{error,no_float} ->
			list_to_integer(N);
		{F,_Rest} ->
			F
		end
	end.

readrecord(S, R, Dbtype) ->
	Country_position = [0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2],
	Region_position = [0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3],
	City_position = [0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4],
	Isp_position = [0, 0, 3, 0, 5, 0, 7, 5, 7, 0, 8, 0, 9, 0, 9, 0, 9, 0, 9, 7, 9, 0, 9, 7, 9, 9, 9],
	Latitude_position = [0, 0, 0, 0, 0, 5, 5, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5],
	Longitude_position = [0, 0, 0, 0, 0, 6, 6, 0, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6],
	Domain_position = [0, 0, 0, 0, 0, 0, 0, 6, 8, 0, 9, 0, 10,0, 10, 0, 10, 0, 10, 8, 10, 0, 10, 8, 10, 10, 10],
	Zipcode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 7, 7, 7, 0, 7, 7, 7, 0, 7, 0, 7, 7, 7, 0, 7, 7, 7],
	Timezone_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 7, 8, 8, 8, 7, 8, 0, 8, 8, 8, 0, 8, 8, 8],
	Netspeed_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 11,0, 11,8, 11, 0, 11, 0, 11, 0, 11, 11, 11],
	Iddcode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 12, 0, 12, 0, 12, 9, 12, 0, 12, 12, 12],
	Areacode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10 ,13 ,0, 13, 0, 13, 10, 13, 0, 13, 13, 13],
	Weatherstationcode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 14, 0, 14, 0, 14, 0, 14, 14, 14],
	Weatherstationname_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 15, 0, 15, 0, 15, 0, 15, 15, 15],
	Mcc_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 16, 0, 16, 9, 16, 16, 16],
	Mnc_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10,17, 0, 17, 10, 17, 17, 17],
	Mobilebrand_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11,18, 0, 18, 11, 18, 18, 18],
	Elevation_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 19, 0, 19, 19, 19],
	Usagetype_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 20, 20, 20],
	Addresstype_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21],
	Category_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 22, 22],
	District_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 23],
	Asn_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 24],
	As_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25],
	
	{Country_short, Country_long} = readcolcountryrow(S, R, Dbtype, Country_position),
	Region = readcolstringrow(S, R, Dbtype, Region_position),
	City = readcolstringrow(S, R, Dbtype, City_position),
	Isp = readcolstringrow(S, R, Dbtype, Isp_position),
	Latitude = readcolfloatrow(R, Dbtype, Latitude_position),
	Longitude = readcolfloatrow(R, Dbtype, Longitude_position),
	Domain = readcolstringrow(S, R, Dbtype, Domain_position),
	Zipcode = readcolstringrow(S, R, Dbtype, Zipcode_position),
	Timezone = readcolstringrow(S, R, Dbtype, Timezone_position),
	Netspeed = readcolstringrow(S, R, Dbtype, Netspeed_position),
	Iddcode = readcolstringrow(S, R, Dbtype, Iddcode_position),
	Areacode = readcolstringrow(S, R, Dbtype, Areacode_position),
	Weatherstationcode = readcolstringrow(S, R, Dbtype, Weatherstationcode_position),
	Weatherstationname = readcolstringrow(S, R, Dbtype, Weatherstationname_position),
	Mcc = readcolstringrow(S, R, Dbtype, Mcc_position),
	Mnc = readcolstringrow(S, R, Dbtype, Mnc_position),
	Mobilebrand = readcolstringrow(S, R, Dbtype, Mobilebrand_position),
	Elevation = readcolfloatstringrow(S, R, Dbtype, Elevation_position),
	Usagetype = readcolstringrow(S, R, Dbtype, Usagetype_position),
	Addresstype = readcolstringrow(S, R, Dbtype, Addresstype_position),
	Category = readcolstringrow(S, R, Dbtype, Category_position),
	District = readcolstringrow(S, R, Dbtype, District_position),
	Asn = readcolstringrow(S, R, Dbtype, Asn_position),
	As = readcolstringrow(S, R, Dbtype, As_position),
	
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
	usagetype = Usagetype,
	addresstype = Addresstype,
	category = Category,
	district = District,
	asn = Asn,
	as = As
	}.

searchtree(S, Ipnum, Dbtype, Low, High, BaseAddr, Colsize, Iptype) ->
	if
		Low =< High ->
			Mid = ((Low + High) bsr 1),
			Rowoffset = BaseAddr + (Mid * Colsize),
			
			case Iptype of
			ipv4 ->
				Firstcol = 4; % 4 bytes
			ipv6 ->
				Firstcol = 16 % 16 bytes
			end,
			
			Readlen = Colsize + Firstcol,
			case file:pread(S, Rowoffset - 1, Readlen) of % reading IP From + whole row + next IP From
			eof ->
				io:format("Error: IP address not found.~n", []),
				{}; % return empty
			{ok, R} ->
				if
					Iptype == ipv4 ->
						Ipfrom = readuint32row(R, 0),
						Ipto = readuint32row(R, Colsize);
					true ->
						Ipfrom = readuint128row(R, 0),
						Ipto = readuint128row(R, Colsize)
				end,
				
				if
					Ipnum >= Ipfrom andalso Ipnum < Ipto ->
						Rowlen = Colsize - Firstcol,
						R2 = binary:part(R, Firstcol, Rowlen),
						
						readrecord(S, R2, Dbtype + 1);
					true ->
						if
							Ipnum < Ipfrom ->
								searchtree(S, Ipnum, Dbtype, Low, Mid - 1, BaseAddr, Colsize, Iptype);
							true ->
								searchtree(S, Ipnum, Dbtype, Mid + 1, High, BaseAddr, Colsize, Iptype)
						end
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
			case file:pread(S, Indexpos - 1, 8) of % 4 bytes for each IP From & IP To
			eof ->
				io:format("Error: IP address not found.~n", []),
				{}; % return empty
			{ok, R} ->
				Low2 = readuint32row(R, 0),
				High2 = readuint32row(R, 4),
				searchtree(S, Ipnum, Dbtype, Low2, High2, Baseaddr, Colsize, ipv4)
			end;
		true ->
			searchtree(S, Ipnum, Dbtype, Low, High, Baseaddr, Colsize, ipv4)
	end.

search6(S, Ipnum, Dbtype, Low, High, Baseaddr, Indexbaseaddr, Colsize) ->
	if
		Indexbaseaddr > 0 ->
			Indexpos = ((Ipnum bsr 112) bsl 3) + Indexbaseaddr,
			case file:pread(S, Indexpos - 1, 8) of % 4 bytes for each IP From & IP To
			eof ->
				io:format("Error: IP address not found.~n", []),
				{}; % return empty
			{ok, R} ->
				Low2 = readuint32row(R, 0),
				High2 = readuint32row(R, 4),
				searchtree(S, Ipnum, Dbtype, Low2, High2, Baseaddr, Colsize, ipv6)
			end;
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
				if
					Ipnum == 4294967295 ->
						Ipnum2 = Ipnum - 1;
					true ->
						Ipnum2 = Ipnum
				end,
				search4(S, Ipnum2, Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
			{ok, {X1, X2, X3, X4, X5, X6, X7, X8}} ->
				Ipnum = (X1 bsl 112) + (X2 bsl 96) + (X3 bsl 80) + (X4 bsl 64) + (X5 bsl 48) + (X6 bsl 32) + (X7 bsl 16) + X8,
				if
					Ipnum >= Fromv4mapped andalso Ipnum =< Tov4mapped ->
						Ipnum2 = Ipnum - Fromv4mapped,
						if
							Ipnum2 == 4294967295 ->
								Ipnum3 = Ipnum2 - 1;
							true ->
								Ipnum3 = Ipnum2
						end,
						search4(S, Ipnum3, Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
					Ipnum >= From6to4 andalso Ipnum =< To6to4 ->
						Ipnum2 = (Ipnum bsr 80) band Last32bits,
						if
							Ipnum2 == 4294967295 ->
								Ipnum3 = Ipnum2 - 1;
							true ->
								Ipnum3 = Ipnum2
						end,
						search4(S, Ipnum3, Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
					Ipnum >= Fromteredo andalso Ipnum =< Toteredo ->
						Ipnum2 = (bnot Ipnum) band Last32bits,
						if
							Ipnum2 == 4294967295 ->
								Ipnum3 = Ipnum2 - 1;
							true ->
								Ipnum3 = Ipnum2
						end,
						search4(S, Ipnum3, Databasetype, 0, Ipv4databasecount, Ipv4databaseaddr, Ipv4indexbaseaddr, Ipv4columnsize);
					true ->
						if
							Ipv6databasecount > 0 ->
								if
									Ipnum == 340282366920938463463374607431768211455 ->
										Ipnum2 = Ipnum - 1;
									true ->
										Ipnum2 = Ipnum
								end,
								search6(S, Ipnum2, Databasetype, 0, Ipv6databasecount, Ipv6databaseaddr, Ipv6indexbaseaddr, Ipv6columnsize);
							true ->
								io:format("Error: IPv6 address is missing in IPv4 BIN.~n", []),
								{} % return empty
						end
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

closews() ->
	case ets:info(myws) of
		undefined ->
			ok;
		_ ->
			ets:delete(myws),
			ok
	end.

configurews(APIKey, APIPackage, UseSSL) ->
	_ = closews(),
	
	case ets:info(myws) of
		undefined ->
			ets:new(myws, [set, named_table]),
			ets:insert(myws, {apikey, APIKey}),
			ets:insert(myws, {apipackage, APIPackage}),
			ets:insert(myws, {usessl, UseSSL}),
			ok;
		_ ->
			ok
	end.

checkparams(APIKey, APIPackage) ->
	RegExp = "^[\\dA-Z]{10}$",
	RegExp2 = "^WS\\d+$",
	case re:run(APIKey, RegExp) of
		{match, _} ->
			case re:run(APIPackage, RegExp2) of
				nomatch ->
					io:format("Invalid package name.~n", []),
					halt();
				{match, _} ->
					ok % do nothing
			end;
		nomatch ->
			io:format("Invalid API key.~n", []),
			halt()
	end.

openws(APIKey, APIPackage, UseSSL) ->
	case checkparams(APIKey, APIPackage) of
		ok ->
			case UseSSL of
				false ->
					configurews(APIKey, APIPackage, UseSSL);
				_ ->
					configurews(APIKey, APIPackage, true)
			end;
		_ ->
			-1 % should have been halted in checkparams
	end.

lookup(IPAddress, AddOn, Lang) ->
	ssl:start(),
	inets:start(),
	
	case ets:info(myws) of
		undefined ->
			io:format("Run openws first.~n", []),
			halt();
		_ ->
			case ets:lookup(myws, apikey) of
				[] ->
					io:format("Run openws first.~n", []),
					halt();
				[{_, APIKey}] ->
					case ets:lookup(myws, apipackage) of
						[] ->
							io:format("Run openws first.~n", []),
							halt();
						[{_, APIPackage}] ->
							case ets:lookup(myws, usessl) of
								[] ->
									io:format("Run openws first.~n", []),
									halt();
								[{_, UseSSL}] ->
									case UseSSL of
										true ->
											Protocol = "https";
										_ ->
											Protocol = "http"
									end,
									MyParams = uri_string:compose_query([{"key", APIKey}, {"package", APIPackage}, {"ip", IPAddress}, {"addon", AddOn}, {"lang", Lang}]),
									
									case httpc:request(get, {Protocol ++ "://api.ip2location.com/v2/?" ++ MyParams, []}, [{ssl, [{versions, ['tlsv1.2']}]}, {autoredirect, false}], []) of
										{ok, {{_, 200, _}, _, Body}} ->
											jiffy:decode(unicode:characters_to_binary(Body,unicode,utf8),[return_maps]);
										{error, Reason} ->
											{error, Reason}
									end
							end
					end
			end
	end.

getcredit() ->
	ssl:start(),
	inets:start(),
	
	case ets:info(myws) of
		undefined ->
			io:format("Run openws first.~n", []),
			halt();
		_ ->
			case ets:lookup(myws, apikey) of
				[] ->
					io:format("Run openws first.~n", []),
					halt();
				[{_, APIKey}] ->
					case ets:lookup(myws, usessl) of
						[] ->
							io:format("Run openws first.~n", []),
							halt();
						[{_, UseSSL}] ->
							case UseSSL of
								true ->
									Protocol = "https";
								_ ->
									Protocol = "http"
							end,
							MyParams = uri_string:compose_query([{"key", APIKey}, {"check", "true"}]),
							
							case httpc:request(get, {Protocol ++ "://api.ip2location.com/v2/?" ++ MyParams, []}, [{ssl, [{versions, ['tlsv1.2']}]}, {autoredirect, false}], []) of
								{ok, {{_, 200, _}, _, Body}} ->
									jiffy:decode(unicode:characters_to_binary(Body,unicode,utf8),[return_maps]);
								{error, Reason} ->
									{error, Reason}
							end
					end
			end
	end.
