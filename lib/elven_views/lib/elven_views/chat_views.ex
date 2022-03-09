defmodule ElvenViews.ChatViews do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenViews

  alias ElvenCaching.Entity
  alias ElvenViews.ChatPackets.{BnPacket, SayPacket}

  ## Public API

  @impl true
  def render(:bn, args) do
    id = required_param(args, :id)
    message = required_param(args, :message)

    %BnPacket{id: id, message: message}
  end

  # Used by defcommand/2 macro
  def render(:say, %{entity_type: entity_type, entity_id: entity_id, message: message} = args) do
    %SayPacket{
      entity_type: entity_type,
      entity_id: entity_id,
      color: Map.get(args, :color, :default),
      message: message
    }
  end

  def render(:say, args) do
    entity = required_param(args, :entity)
    message = required_param(args, :message)
    color = optional_param(args, :color)

    %SayPacket{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      color: color,
      message: message
    }
  end
end
