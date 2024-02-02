# IP2Location Erlang API

## ip2location Class

```{py:function} new(InputFile)
Open and load the IP2Location BIN database for lookup.

:param String InputFile: (Required) The file path links to IP2Location BIN databases.
```

```{py:function} close()
Closes BIN file and resets metadata.
```

```{py:function} getapiversion()
Return the IP2Location Erlang module version.
```

```{py:function} query(Ip)
Retrieve geolocation information for an IP address.

:param String Ip: (Required) The IP address (IPv4 or IPv6).
:return: Returns the geolocation information. Refer below table for the fields avaliable.

**RETURN FIELDS**

| Field Name       | Description                                                  |
| ---------------- | ------------------------------------------------------------ |
| Country_short    |     Two-character country code based on ISO 3166. |
| Country_long     |     Country name based on ISO 3166. |
| Region           |     Region or state name. |
| City             |     City name. |
| Isp              |     Internet Service Provider or company\'s name. |
| Latitude         |     City latitude. Defaults to capital city latitude if city is unknown. |
| Longitude        |     City longitude. Defaults to capital city longitude if city is unknown. |
| Domain           |     Internet domain name associated with IP address range. |
| Zipcode          |     ZIP code or Postal code. [172 countries supported](https://www.ip2location.com/zip-code-coverage). |
| Timezone         |     UTC time zone (with DST supported). |
| Netspeed         |     Internet connection type. |
| Iddcode         |     The IDD prefix to call the city from another country. |
| Areacode        |     A varying length number assigned to geographic areas for calls between cities. [223 countries supported](https://www.ip2location.com/area-code-coverage). |
| Weatherstationcode     |     The special code to identify the nearest weather observation station. |
| Weatherstationname     |     The name of the nearest weather observation station. |
| Mcc              |     Mobile Country Codes (MCC) as defined in ITU E.212 for use in identifying mobile stations in wireless telephone networks, particularly GSM and UMTS networks. |
| Mnc              |     Mobile Network Code (MNC) is used in combination with a Mobile Country Code(MCC) to uniquely identify a mobile phone operator or carrier. |
| Mobilebrand     |     Commercial brand associated with the mobile carrier. You may click [mobile carrier coverage](https://www.ip2location.com/mobile-carrier-coverage) to view the coverage report. |
| Elevation        |     Average height of city above sea level in meters (m). |
| Usagetype       |     Usage type classification of ISP or company. |
| Addresstype     |     IP address types as defined in Internet Protocol version 4 (IPv4) and Internet Protocol version 6 (IPv6). |
| Category         |     The domain category based on [IAB Tech Lab Content Taxonomy](https://www.ip2location.com/free/iab-categories). |
| District         |     District or county name. |
| Asn              |     Autonomous system number (ASN). BIN databases. |
| As          |     Autonomous system (AS) name. |
```
