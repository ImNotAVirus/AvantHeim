defmodule ChannelService.Endpoint.ChatViews do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.Entity.Character
  alias ChannelService.Endpoint.ChatPackets.{Bn, Say}

  ## Public API

  @spec render(atom, any) :: any
  def render(:bn, %{id: id, message: message}), do: %Bn{id: id, message: message}

  def render(:say, %{entity_type: entity_type, entity_id: entity_id, message: message} = attrs) do
    %Say{
      entity_type: entity_type,
      entity_id: entity_id,
      color: Map.get(attrs, :color, :default),
      message: message
    }
  end

  # TODO: Add clauses for monsters/npc/mates, ....
  def render(:say, %{entity: %Character{} = entity, message: message} = attrs) do
    %Say{
      entity_type: :character,
      entity_id: entity.id,
      color: Map.get(attrs, :color, :default),
      message: message
    }
  end
end
