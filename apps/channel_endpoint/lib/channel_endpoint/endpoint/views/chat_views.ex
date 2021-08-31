defmodule ChannelEndpoint.Endpoint.ChatViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.ChatPackets.{Bn, Say, Sayi2}

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
  def render(:say, %{entity: %Character{id: id}, message: message} = attrs) do
    %Say{
      entity_type: :character,
      entity_id: id,
      color: Map.get(attrs, :color, :default),
      message: message
    }
  end

  def render(:sayi2, %{
        entity: %Character{id: id},
        color: color,
        i18n_vnum: i18n_vnum,
        params_count: params_count,
        name: name
      }) do
    %Sayi2{
      entity_type: :character,
      entity_id: id,
      color: color,
      i18n_vnum: i18n_vnum,
      params_count: params_count,
      name: name
    }
  end
end
