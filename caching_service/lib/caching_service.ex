defmodule CachingService do
  @moduledoc """
  Documentation for `CachingService`.
  """

  require ElvenEnums.EntityEnums

  alias ElvenEnums.EntityEnums
  alias CachingService.CharacterRegistry

  @type entity :: Character.t()

  ## Public API

  @spec get_entity_by_id(EntityEnums.entity_type(), pos_integer) ::
          {:ok, entity()} | {:error, any}
  def get_entity_by_id(entity_type_val, entity_id) do
    case EntityEnums.entity_type(entity_type_val, :key) do
      :character -> CharacterRegistry.get(entity_id)
      :monster -> raise "TODO: unsupported entity type"
      :npc -> raise "TODO: unsupported entity type"
      _ -> {:error, :unknown_entity_type}
    end
  end
end
