defprotocol ElvenCaching.Entity do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.Entity.EntityPosition

  @spec get_position(t()) :: EntityPosition.t()
  def get_position(entity)

  @spec set_position(t(), EntityPosition.t()) :: t()
  def set_position(entity, position)
end
