defprotocol ElvenCaching.MapEntity do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.Entity.EntityPosition
  alias ElvenData.Enums.EntityEnums

  @type position :: EntityPosition.t()
  @type direction :: EntityEnums.direction_type_keys()
  @type speed :: non_neg_integer
  @type size :: pos_integer

  @doc "Get the MapEntity position"
  @spec position(t()) :: position()
  def position(entity)

  @doc "Set the MapEntity position"
  @spec position(t(), position()) :: t()
  def position(entity, position)

  @doc "Get the MapEntity direction"
  @spec direction(t()) :: direction()
  def direction(entity)

  @doc "Set the MapEntity direction"
  @spec direction(t(), direction()) :: t()
  def direction(entity, direction)

  @doc "Get the MapEntity is_sitting"
  @spec is_sitting(t()) :: boolean
  def is_sitting(entity)

  @doc "Set the MapEntity is_sitting"
  @spec is_sitting(t(), boolean) :: t()
  def is_sitting(entity, is_sitting)

  @doc "Get the MapEntity speed"
  @spec speed(t()) :: speed()
  def speed(entity)

  @doc "Set the MapEntity speed"
  @spec speed(t(), speed()) :: t()
  def speed(entity, speed)

  @doc "Get the MapEntity size"
  @spec size(t()) :: size()
  def size(entity)

  @doc "Set the MapEntity size"
  @spec size(t(), size()) :: t()
  def size(entity, size)
end
