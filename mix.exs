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
    "Query country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain, connection type, IDD, area code, weather station, mcc, mnc, mobile brand, elevation, usage type, address type, IAB category, district, (ASN) and (AS) from IP address by using IP2Location database."
  end

  defp package() do
    [
      # This option is only needed when you don't want to use the OTP application name
      name: "ip2location-erlang",
      # These are the default files included in the package
      files: ~w(mix.exs README* LICENSE* *.erl),
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/ip2location/ip2location-erlang"}
    ]
  end
end