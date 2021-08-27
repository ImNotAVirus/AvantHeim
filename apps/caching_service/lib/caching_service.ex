defmodule CachingService do
  @moduledoc """
  Documentation for `CachingService`.
  """

  defdelegate init_character(character, socket), to: CachingService.CharacterRegistry
  defdelegate write_character(character), to: CachingService.CharacterRegistry
  defdelegate get_character_by_id(id), to: CachingService.CharacterRegistry
  defdelegate delete_character_by_id(id), to: CachingService.CharacterRegistry
  defdelegate get_character_by_name(name), to: CachingService.CharacterRegistry

  defdelegate get_characters_by_map_id(map_id, except_guards \\ []),
    to: CachingService.CharacterRegistry
end
