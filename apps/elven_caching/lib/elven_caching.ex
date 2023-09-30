defmodule ElvenCaching do
  @moduledoc """
  Documentation for `ElvenCaching`.
  """

  ## Internal API

  @doc false
  def create_table!(table, opts \\ []) do
    case Memento.Table.create(table, opts) do
      :ok -> :ok
      {:error, {:already_exists, ^table}} -> :ok
      error -> raise "can't create table: #{inspect(error)}"
    end
  end
end
