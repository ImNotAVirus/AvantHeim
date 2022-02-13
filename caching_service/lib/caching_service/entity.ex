defprotocol CachingService.Entity do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Entity.EntityPosition

  @spec get_position(t()) :: EntityPosition.t()
  def get_position(entity)

  @spec set_position(t(), EntityPosition.t()) :: t()
  def set_position(entity, position)
end
