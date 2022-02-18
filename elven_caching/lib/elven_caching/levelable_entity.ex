defprotocol ElvenCaching.LevelableEntity do
  @moduledoc """
  TODO: Documentation
  """

  ## TODO: Maybe also job level and all level_xp

  @type level :: non_neg_integer()

  @doc "Get the LevelableEntity level"
  @spec level(t()) :: level()
  def level(entity)

  @doc "Set the LevelableEntity level"
  @spec level(t(), level()) :: t()
  def level(entity, level)

  @doc "Get the LevelableEntity hero_level"
  @spec hero_level(t()) :: level()
  def hero_level(entity)

  @doc "Set the LevelableEntity hero_level"
  @spec hero_level(t(), level()) :: t()
  def hero_level(entity, hero_level)
end
