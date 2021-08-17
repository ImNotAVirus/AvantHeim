defmodule ChannelEndpoint.Endpoint.ChatViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.ChatPackets.{Bn, Say}

  ## Public API

  @spec render(atom, any) :: any
  def render(:bn, %{id: id, message: message}), do: %Bn{id: id, message: message}

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
