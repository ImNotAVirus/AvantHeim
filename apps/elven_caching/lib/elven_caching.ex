defmodule ElvenCaching do
  @moduledoc """
  Documentation for `ElvenCaching`.
  """

  require ElvenEnums.EntityEnums

  alias ElvenEnums.EntityEnums
  alias ElvenCaching.CharacterRegistry

  @type entity :: Character.t()

  ## Public API

  @spec get_entity_by_id(EntityEnums.entity_type(), pos_integer) ::
          {:ok, entity()} | {:error, :not_found}
  def get_entity_by_id(entity_type_val, entity_id) do
    case EntityEnums.entity_type(entity_type_val, :key) do
      :player -> CharacterRegistry.get(entity_id)
      :monster -> raise "TODO: unsupported entity type"
      :npc -> raise "TODO: unsupported entity type"
      _ -> {:error, :unknown_entity_type}
    end
  end

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
