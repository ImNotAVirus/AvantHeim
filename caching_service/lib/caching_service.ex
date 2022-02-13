defmodule CachingService do
  @moduledoc """
  Documentation for `CachingService`.
  """

  import ElvenEnums.EntityEnums, only: [entity_type: 2]

  alias ElvenEnums.EntityEnums

  @type entity :: Character.t()

  ## Delegates

  defdelegate init_character(character, socket), to: CachingService.CharacterRegistry
  defdelegate write_character(character), to: CachingService.CharacterRegistry
  defdelegate get_character_by_id(id), to: CachingService.CharacterRegistry
  defdelegate delete_character_by_id(id), to: CachingService.CharacterRegistry
  defdelegate get_character_by_name(name), to: CachingService.CharacterRegistry

  defdelegate get_characters_by_map_id(map_id, except_guards \\ []),
    to: CachingService.CharacterRegistry

  ## Public API

  @spec get_entity_by_id(EntityEnums.entity_type(), pos_integer) ::
          {:ok, entity()} | {:error, any}
  def get_entity_by_id(entity_type_val, entity_id) do
    case entity_type(entity_type_val, :key) do
      :character -> CachingService.get_character_by_id(entity_id)
      :monster -> raise "TODO: unsupported entity type"
      :npc -> raise "TODO: unsupported entity type"
      _ -> {:error, :unknown_entity_type}
    end
  end
end
