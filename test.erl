-module(test).
-export([testme/0]).

printme(V) ->
	case V of
		{} ->
			io:format("No results.~n", []);
		{ip2locationrecord, Country_short, Country_long, Region, City, Isp, Latitude, Longitude, Domain, Zipcode, Timezone, Netspeed, Iddcode, Areacode, Weatherstationcode, Weatherstationname, Mcc, Mnc, Mobilebrand, Elevation, Usagetype, Addresstype, Category} ->
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
			io:format("Usagetype: ~p~n", [Usagetype]),
			io:format("Addresstype: ~p~n", [Addresstype]),
			io:format("Category: ~p~n", [Category])
	end,
	io:format("===================================================================~n", []).

testme() ->
	ip2location:new("IPV6-COUNTRY-REGION-CITY-LATITUDE-LONGITUDE-ZIPCODE-TIMEZONE-ISP-DOMAIN-NETSPEED-AREACODE-WEATHER-MOBILE-ELEVATION-USAGETYPE-ADDRESSTYPE-CATEGORY.BIN"),
	ip2location:getapiversion(),
	V1 = ip2location:query("8.8.8.8"),
	printme(V1),
	ip2location:close().

testme2() ->
	APIKey = "YOUR_API_KEY",
	APIPackage = "WS25",
	UseSSL = true,
	IP = "8.8.8.8",
	AddOn = "continent,country,region,city,geotargeting,country_groupings,time_zone_info", % leave blank if no need
	Lang = "fr", % leave blank if no need
	
	ip2location:openws(APIKey, APIPackage, UseSSL),
	Result = ip2location:lookup(IP, AddOn, Lang),
	case Result of
		{error, Reason} ->
			io:format("Error: ~p~n", [Reason]);
		_ ->
			case maps:is_key(<<"response">>, Result) of
				true ->
					Response = maps:get(<<"response">>, Result),
					case Response of
						<<"OK">> ->
							% standard results
							io:format("country_code: ~p~n", [maps:get(<<"country_code">>, Result)]),
							io:format("country_name: ~p~n", [maps:get(<<"country_name">>, Result)]),
							io:format("region_name: ~p~n", [maps:get(<<"region_name">>, Result)]),
							io:format("city_name: ~p~n", [maps:get(<<"city_name">>, Result)]),
							io:format("latitude: ~p~n", [maps:get(<<"latitude">>, Result)]),
							io:format("longitude: ~p~n", [maps:get(<<"longitude">>, Result)]),
							io:format("longitude: ~p~n", [maps:get(<<"longitude">>, Result)]),
							io:format("zip_code: ~p~n", [maps:get(<<"zip_code">>, Result)]),
							io:format("time_zone: ~p~n", [maps:get(<<"time_zone">>, Result)]),
							io:format("isp: ~p~n", [maps:get(<<"isp">>, Result)]),
							io:format("domain: ~p~n", [maps:get(<<"domain">>, Result)]),
							io:format("net_speed: ~p~n", [maps:get(<<"net_speed">>, Result)]),
							io:format("idd_code: ~p~n", [maps:get(<<"idd_code">>, Result)]),
							io:format("area_code: ~p~n", [maps:get(<<"area_code">>, Result)]),
							io:format("weather_station_code: ~p~n", [maps:get(<<"weather_station_code">>, Result)]),
							io:format("weather_station_name: ~p~n", [maps:get(<<"weather_station_name">>, Result)]),
							io:format("mcc: ~p~n", [maps:get(<<"mcc">>, Result)]),
							io:format("mnc: ~p~n", [maps:get(<<"mnc">>, Result)]),
							io:format("mobile_brand: ~p~n", [maps:get(<<"mobile_brand">>, Result)]),
							io:format("elevation: ~p~n", [maps:get(<<"elevation">>, Result)]),
							io:format("usage_type: ~p~n", [maps:get(<<"usage_type">>, Result)]),
							io:format("address_type: ~p~n", [maps:get(<<"address_type">>, Result)]),
							io:format("category: ~p~n", [maps:get(<<"category">>, Result)]),
							io:format("category_name: ~p~n", [maps:get(<<"category_name">>, Result)]),
							io:format("credits_consumed: ~p~n", [maps:get(<<"credits_consumed">>, Result)]),
							
							% continent addon
							case maps:is_key(<<"continent">>, Result) of
								true ->
									Continent = maps:get(<<"continent">>, Result),
									io:format("continent => name: ~p~n", [maps:get(<<"name">>, Continent)]),
									io:format("continent => code: ~p~n", [maps:get(<<"code">>, Continent)]),
									io:format("continent => hemisphere: ~p~n", [maps:get(<<"hemisphere">>, Continent)]),
									case maps:is_key(<<"translations">>, Continent) of
										true ->
											io:format("continent => translations: ~p~n", [maps:get(<<"translations">>, Continent)]);
										_ ->
											""
									end;
								_ ->
									""
							end,
							
							% country addon
							case maps:is_key(<<"country">>, Result) of
								true ->
									Country = maps:get(<<"country">>, Result),
									io:format("country => name: ~p~n", [maps:get(<<"name">>, Country)]),
									io:format("country => alpha3_code: ~p~n", [maps:get(<<"alpha3_code">>, Country)]),
									io:format("country => numeric_code: ~p~n", [maps:get(<<"numeric_code">>, Country)]),
									io:format("country => demonym: ~p~n", [maps:get(<<"demonym">>, Country)]),
									io:format("country => flag: ~p~n", [maps:get(<<"flag">>, Country)]),
									io:format("country => capital: ~p~n", [maps:get(<<"capital">>, Country)]),
									io:format("country => total_area: ~p~n", [maps:get(<<"total_area">>, Country)]),
									io:format("country => population: ~p~n", [maps:get(<<"population">>, Country)]),
									io:format("country => idd_code: ~p~n", [maps:get(<<"idd_code">>, Country)]),
									io:format("country => tld: ~p~n", [maps:get(<<"tld">>, Country)]),
									io:format("country => is_eu: ~p~n", [maps:get(<<"is_eu">>, Country)]),
									
									CountryCurrency = maps:get(<<"currency">>, Country),
									io:format("country => currency => code: ~p~n", [maps:get(<<"code">>, CountryCurrency)]),
									io:format("country => currency => name: ~p~n", [maps:get(<<"name">>, CountryCurrency)]),
									io:format("country => currency => symbol: ~p~n", [maps:get(<<"symbol">>, CountryCurrency)]),
									
									CountryLanguage = maps:get(<<"language">>, Country),
									io:format("country => language => code: ~p~n", [maps:get(<<"code">>, CountryLanguage)]),
									io:format("country => language => name: ~p~n", [maps:get(<<"name">>, CountryLanguage)]),
									
									case maps:is_key(<<"translations">>, Country) of
										true ->
											io:format("country => translations: ~p~n", [maps:get(<<"translations">>, Country)]);
										_ ->
											""
									end;
								_ ->
									""
							end,
							
							% region addon
							case maps:is_key(<<"region">>, Result) of
								true ->
									Region = maps:get(<<"region">>, Result),
									io:format("region => name: ~p~n", [maps:get(<<"name">>, Region)]),
									io:format("region => code: ~p~n", [maps:get(<<"code">>, Region)]),
									case maps:is_key(<<"translations">>, Region) of
										true ->
											io:format("region => translations: ~p~n", [maps:get(<<"translations">>, Region)]);
										_ ->
											""
									end;
								_ ->
									""
							end,
							
							% city addon
							case maps:is_key(<<"city">>, Result) of
								true ->
									City = maps:get(<<"city">>, Result),
									io:format("city => name: ~p~n", [maps:get(<<"name">>, City)]),
									case maps:is_key(<<"translations">>, City) of
										true ->
											io:format("city => translations: ~p~n", [maps:get(<<"translations">>, City)]);
										_ ->
											""
									end;
								_ ->
									""
							end,
							
							% geotargeting addon
							case maps:is_key(<<"geotargeting">>, Result) of
								true ->
									Geotargeting = maps:get(<<"geotargeting">>, Result),
									io:format("geotargeting => metro: ~p~n", [maps:get(<<"metro">>, Geotargeting)]);
								_ ->
									""
							end,
							
							% country_groupings addon
							case maps:is_key(<<"country_groupings">>, Result) of
								true ->
									Groupings = maps:get(<<"country_groupings">>, Result),
									io:format("country_groupings: ~p~n", [Groupings]);
								_ ->
									""
							end,
							
							% time_zone_info addon
							case maps:is_key(<<"time_zone_info">>, Result) of
								true ->
									TimeZone = maps:get(<<"time_zone_info">>, Result),
									io:format("time_zone_info => olson: ~p~n", [maps:get(<<"olson">>, TimeZone)]),
									io:format("time_zone_info => current_time: ~p~n", [maps:get(<<"current_time">>, TimeZone)]),
									io:format("time_zone_info => gmt_offset: ~p~n", [maps:get(<<"gmt_offset">>, TimeZone)]),
									io:format("time_zone_info => is_dst: ~p~n", [maps:get(<<"is_dst">>, TimeZone)]),
									io:format("time_zone_info => sunrise: ~p~n", [maps:get(<<"sunrise">>, TimeZone)]),
									io:format("time_zone_info => sunset: ~p~n", [maps:get(<<"sunset">>, TimeZone)]);
								_ ->
									""
							end;
						_ ->
							io:format("Error: ~p~n", [Response])
					end;
				_ ->
					""
			end
	end,
	
	Result2 = ip2location:getcredit(),
	case Result2 of
		{error, Reason2} ->
			io:format("Error: ~p~n", [Reason2]);
		_ ->
			case maps:is_key(<<"response">>, Result2) of
				true ->
					io:format("Credit Balance: ~p~n", [maps:get(<<"response">>, Result2)]);
				_ ->
					""
			end
	end.