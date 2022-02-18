defmodule ChannelService.Endpoint.ChatViews do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.Entity

  alias ChannelService.Endpoint.ChatPackets.{Bn, Say}

  ## Public API

  @spec render(atom, any) :: any
  def render(:bn, %{id: id, message: message}), do: %Bn{id: id, message: message}

  # Used by defcommand/2 macro
  def render(:say, %{entity_type: entity_type, entity_id: entity_id, message: message} = attrs) do
    %Say{
      entity_type: entity_type,
      entity_id: entity_id,
      color: Map.get(attrs, :color, :default),
      message: message
    }
  end

  # TODO: Add clauses for monsters/npc/mates, ....
  def render(:say, %{entity: entity, message: message} = attrs) do
    %Say{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      color: Map.get(attrs, :color, :default),
      message: message
    }
  end
end
