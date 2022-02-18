defprotocol ElvenCaching.Entity do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenEnums.EntityEnums

  @type entity_type :: EntityEnums.entity_type_keys()
  @type entity_id :: pos_integer()

  @doc "Returns the Entity type"
  @spec type(t()) :: entity_type()
  def type(entity)

  @doc "Returns the Entity id"
  @spec id(t()) :: entity_id()
  def id(entity)
end
