defprotocol ElvenCaching.MapEntity do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.Entity.EntityPosition

  @doc "Returns the MapEntity position"
  @spec position(t()) :: EntityPosition.t()
  def position(entity)

  @doc "Set the MapEntity position"
  @spec position(t(), EntityPosition.t()) :: t()
  def position(entity, position)
end
