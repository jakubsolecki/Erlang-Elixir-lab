defmodule PollutionDataStream do
  @moduledoc false

  def importLinesFromCSV() do
    File.stream!("pollution.csv") |> Stream.map(&parseLine(&1)) |> Enum.to_list
  end

  def parseLine(line) do
    [date, time, lon, lat, value] = String.split(line, ",")
    date = String.split(date, "-") |> Enum.reverse |> Stream.map(&(Integer.parse(&1) |> elem(0))) |>
      Enum.reduce({}, fn(element, tuple) -> Tuple.append(tuple, element) end)
    time = String.split(time, ":") |> Stream.map(&(Integer.parse(&1) |> elem(0))) |>
      Enum.reduce({}, fn(element, tuple) -> Tuple.append(tuple, element) end) |> Tuple.append(0)
    lon = Float.parse(lon) |> elem(0)
    lat = Float.parse(lat) |> elem(0)
    value = Integer.parse(value) |> elem(0)

    %{:datetime => {date, time}, :location => {lon, lat}, :pollutionLevel => value}
  end

  def identifyStations(list), do:
    Enum.uniq_by(list, &(&1.location)) |> Enum.map(&(&1.location))

  def addStations([]), do: :ok
  def addStations([location | tail]) do
    stationName = 'station_#{location |> elem(0)}_#{location |> elem(1)}'
    :pollution_gen_server.addStation(stationName, location)
    addStations(tail)
  end

  def addValues([]), do: :ok
  def addValues([reading | tail]) do
    :pollution_gen_server.addValues(reading.location, reading.datetime, 'PM10', reading.pollutionLevel)
    addValues(tail)
  end

  def loadData() do
    addStations = fn () -> importLinesFromCSV() |> identifyStations() |> addStations() end
    addValues = fn () -> importLinesFromCSV() |> addValues() end

    IO.puts("#{ addStations |> :timer.tc([]) |> elem(0)}")
    IO.puts("#{ addValues |> :timer.tc([]) |> elem(0)}")
  end

  def timeCheck() do
    stationMean = fn () -> :pollution_gen_server.getStationMean('station_20.06_49.986', 'PM10') end
    dailyMean = fn () -> :pollution_gen_server.getDailyMean({2017, 5, 3}, 'PM10') end
    IO.puts("#{ stationMean |> :timer.tc([]) |> elem(0)}")
    IO.puts("#{ dailyMean |> :timer.tc([]) |> elem(0)}")
  end
end
