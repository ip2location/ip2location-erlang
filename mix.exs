defmodule IP2LocationErlang.MixProject do
  use Mix.Project

  def project() do
    [
      app: :ip2location_erlang,
      version: "8.6.1",
      elixir: "~> 1.0",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      name: "IP2Location-Erlang",
      source_url: "https://github.com/ip2location/ip2location-erlang"
    ]
  end

  def application() do
    []
  end

  defp deps() do
    [
      {:jiffy, "~> 1.1"}
    ]
  end

  defp description() do
    "This Erlang module provides a fast lookup of country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type, IDD code, area code, weather station code, station name, mcc, mnc, mobile brand, elevation, usage type, address type and IAB category from IP address by using IP2Location database. This module uses a file based database available at IP2Location.com. This database simply contains IP blocks as keys, and other information such as country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type, IDD code, area code, weather station code, station name, mcc, mnc, mobile brand, elevation, usage type, address type, IAB category, district, autonomous system number (ASN) and autonomous system (AS) as values. It supports both IP address in IPv4 and IPv6."
  end

  defp package() do
    [
      # This option is only needed when you don't want to use the OTP application name
      name: "ip2location-erlang",
      # These are the default files included in the package
      files: ~w(lib priv .formatter.exs mix.exs README* readme* LICENSE*
                license* CHANGELOG* changelog* src),
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/ip2location/ip2location-erlang"}
    ]
  end
end