defmodule DatabaseService.EctoAuthority do
  @moduledoc """
  TODO: Documentation
  """

  use Ecto.Type

  ## Enum

  # TODO: Write a library
  @authorities [player: 0, game_master: 2, administrator: 10]
  @authorities_rev @authorities
                   |> Enum.map(fn {k, v} -> {v, k} end)
                   |> Enum.into(%{})
                   |> Macro.escape()
  @authorities_keys Keyword.keys(@authorities)
  @authorities_values Keyword.values(@authorities)

  defmacro authority(:__keys__), do: @authorities_keys
  defmacro authority(:__values__), do: @authorities_values

  Enum.each(
    @authorities,
    fn {k, v} ->
      defmacro authority(unquote(k)), do: unquote(v)
      defmacro authority(unquote(v)), do: unquote(k)
      defmacro authority(unquote(k), :key), do: unquote(k)
      defmacro authority(unquote(k), :value), do: unquote(v)
      defmacro authority(unquote(v), :key), do: unquote(k)
      defmacro authority(unquote(v), :value), do: unquote(v)
    end
  )

  defmacro authority(item) do
    quote do
      case unquote(item) do
        x when x in unquote(@authorities_keys) ->
          Keyword.fetch!(unquote(@authorities), x)

        x when x in unquote(@authorities_values) ->
          Map.fetch!(unquote(@authorities_rev), x)

        x ->
          raise "invalid value for #{inspect(unquote(__MODULE__))}.authorities (got #{inspect(x)})"
      end
    end
  end

  defmacro authority(item, arg) do
    quote do
      case {unquote(item), unquote(arg)} do
        {x, :key} when x in unquote(@authorities_keys) ->
          x

        {x, :value} when x in unquote(@authorities_keys) ->
          Keyword.fetch!(unquote(@authorities), x)

        {x, :key} when x in unquote(@authorities_values) ->
          Map.fetch!(unquote(@authorities_rev), x)

        {x, :value} when x in unquote(@authorities_values) ->
          x

        x ->
          raise "invalid value for #{inspect(unquote(__MODULE__))}.authorities (got #{inspect(x)})"
      end
    end
  end

  ## Ecto.Type behaviour

  @impl true
  def type(), do: :integer

  @impl true
  def cast(item) when item in authority(:__values__) do
    {:ok, authority(item, :key)}
  end

  def cast(item) when item in authority(:__keys__), do: {:ok, item}

  def cast(_), do: :error

  @impl true
  def load(data) when data in authority(:__values__) do
    {:ok, authority(data, :key)}
  end

  def load(_), do: :error

  @impl true
  def dump(item) when item in authority(:__keys__), do: {:ok, authority(item, :value)}
  def dump(_), do: :error
end
