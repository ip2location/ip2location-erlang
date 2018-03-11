IP2Location Erlang Module
=========================

This Erlang module provides a fast lookup of country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type, IDD code, area code, weather station code, station name, mcc, mnc, mobile brand, elevation, and usage type from IP address by using IP2Location database. This module uses a file based database available at IP2Location.com. This database simply contains IP blocks as keys, and other information such as country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type, IDD code, area code, weather station code, station name, mcc, mnc, mobile brand, elevation, and usage type as values. It supports both IP address in IPv4 and IPv6.

This module can be used in many types of projects such as:

 - select the geographically closest mirror
 - analyze your web server logs to determine the countries of your visitors
 - credit card fraud detection
 - software export controls
 - display native language and currency
 - prevent password sharing and abuse of service
 - geotargeting in advertisement

The database will be updated in monthly basis for the greater accuracy. Free LITE databases are available at https://lite.ip2location.com/ upon registration.

The paid databases are available at https://www.ip2location.com under Premium subscription package.


Compilation
=======

```
erlc ip2location.erl
erlc test.erl
```

Example
=======

```erlang
test:testme().
```

IP2Location Elixir
==================

This Erlang module can also be included in a MixProject as follows:

```elixir
defp deps do
  [
    {:ip2location, github: "ip2location/ip2location-erlang"}
  ]
end
```

Dependencies
============

The complete database is available at https://www.ip2location.com under subscription package.


IPv4 BIN vs IPv6 BIN
======================
Use the IPv4 BIN file if you just need to query IPv4 addresses.

Use the IPv6 BIN file if you need to query BOTH IPv4 and IPv6 addresses.


Copyright
=========

Copyright (C) 2018 by IP2Location.com, support@ip2location.com
