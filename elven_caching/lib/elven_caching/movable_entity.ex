defprotocol ElvenCaching.MovableEntity do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.Entity.EntityPosition

  @doc "Returns the MovableEntity position"
  @spec position(t()) :: EntityPosition.t()
  def position(entity)

  @doc "Set the MovableEntity position"
  @spec position(t(), EntityPosition.t()) :: t()
  def position(entity, position)
end
