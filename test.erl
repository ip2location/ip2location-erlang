-module(test).
-export([testme/0]).

printme(V) ->
	case V of
		{} ->
			io:format("No results.~n", []);
		{ip2locationrecord, Country_short, Country_long, Region, City, Isp, Latitude, Longitude, Domain, Zipcode, Timezone, Netspeed, Iddcode, Areacode, Weatherstationcode, Weatherstationname, Mcc, Mnc, Mobilebrand, Elevation, Usagetype} ->
			io:format("Country_short: ~p~n", [Country_short]),
			io:format("Country_long: ~p~n", [Country_long]),
			io:format("Region: ~p~n", [Region]),
			io:format("City: ~p~n", [City]),
			io:format("Isp: ~p~n", [Isp]),
			io:format("Latitude: ~p~n", [Latitude]),
			io:format("Longitude: ~p~n", [Longitude]),
			io:format("Domain: ~p~n", [Domain]),
			io:format("Zipcode: ~p~n", [Zipcode]),
			io:format("Timezone: ~p~n", [Timezone]),
			io:format("Netspeed: ~p~n", [Netspeed]),
			io:format("Iddcode: ~p~n", [Iddcode]),
			io:format("Areacode: ~p~n", [Areacode]),
			io:format("Weatherstationcode: ~p~n", [Weatherstationcode]),
			io:format("Weatherstationname: ~p~n", [Weatherstationname]),
			io:format("Mcc: ~p~n", [Mcc]),
			io:format("Mnc: ~p~n", [Mnc]),
			io:format("Mobilebrand: ~p~n", [Mobilebrand]),
			io:format("Elevation: ~p~n", [Elevation]),
			io:format("Usagetype: ~p~n", [Usagetype])
	end,
	io:format("===================================================================~n", []).

testme() ->
	ip2location:new("IPV6-COUNTRY-REGION-CITY-LATITUDE-LONGITUDE-ZIPCODE-TIMEZONE-ISP-DOMAIN-NETSPEED-AREACODE-WEATHER-MOBILE-ELEVATION-USAGETYPE.BIN"),
	ip2location:getapiversion(),
	V1 = ip2location:query("8.8.8.8"),
	printme(V1),
	ip2location:close().
