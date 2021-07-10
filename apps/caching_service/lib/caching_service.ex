defmodule CachingService do
  @moduledoc """
  Documentation for `CachingService`.
  """

  defdelegate init_character(character), to: CachingService.CharacterRegistry
  defdelegate write_character(character), to: CachingService.CharacterRegistry
  defdelegate get_character_by_id(id), to: CachingService.CharacterRegistry
  defdelegate delete_character_by_id(id), to: CachingService.CharacterRegistry
end
