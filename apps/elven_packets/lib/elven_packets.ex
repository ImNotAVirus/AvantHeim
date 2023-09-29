defmodule ElvenPackets do
  @moduledoc """
  Documentation for `ElvenPackets`.
  """

  def separator(), do: [" ", "\v"]

  @doc """
  ## Examples

      iex> ElvenPackets.format_number(1)
      "1"

      iex> ElvenPackets.format_number(123)
      "123"

      iex> ElvenPackets.format_number(1234)
      "1,234"

      iex> ElvenPackets.format_number(123456789)
      "123,456,789"

      iex> ElvenPackets.format_number(-123456789)
      "-123,456,789"

      iex> ElvenPackets.format_number(12345.6789)
      "12,345.6789"

      iex> ElvenPackets.format_number(-12345.6789)
      "-12,345.6789"

      iex> ElvenPackets.format_number(123456789, thousands_separator: "")
      "123456789"

      iex> ElvenPackets.format_number(-123456789, thousands_separator: "")
      "-123456789"

      iex> ElvenPackets.format_number(12345.6789, thousands_separator: "")
      "12345.6789"

      iex> ElvenPackets.format_number(-12345.6789, thousands_separator: "")
      "-12345.6789"

      iex> ElvenPackets.format_number(123456789, decimal_separator: ",", thousands_separator: ".")
      "123.456.789"

      iex> ElvenPackets.format_number(-123456789, decimal_separator: ",", thousands_separator: ".")
      "-123.456.789"

      iex> ElvenPackets.format_number(12345.6789, decimal_separator: ",", thousands_separator: ".")
      "12.345,6789"

      iex> ElvenPackets.format_number(-12345.6789, decimal_separator: ",", thousands_separator: ".")
      "-12.345,6789"
  """

  @regex ~r/(?<sign>-?)(?<int>\d+)(\.(?<frac>\d+))?/

  def format_number(number, options \\ []) do
    thousands_separator = Keyword.get(options, :thousands_separator, ",")
    parts = Regex.named_captures(@regex, to_string(number))

    formatted_int =
      parts["int"]
      |> String.graphemes()
      |> Enum.reverse()
      |> Enum.chunk_every(3)
      |> Enum.join(thousands_separator)
      |> String.reverse()

    decimal_separator =
      if parts["frac"] == "" do
        ""
      else
        Keyword.get(options, :decimal_separator, ".")
      end

    to_string([parts["sign"], formatted_int, decimal_separator, parts["frac"]])
  end
end
