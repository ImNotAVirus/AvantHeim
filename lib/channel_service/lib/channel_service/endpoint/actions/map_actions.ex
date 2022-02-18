defmodule ChannelService.Endpoint.MapActions do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCore.Socket
  alias ElvenCaching.CharacterRegistry
  alias ElvenCaching.MapEntity
  alias ElvenCaching.Entity.EntityPosition

  alias ChannelService.Endpoint.EntityInteractions
  alias ChannelService.Endpoint.EntityViews

  ## Packet handlers

  @spec dir(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def dir("dir", params, %Socket{} = socket) do
    %{dir: dir, entity_type: entity_type, entity_id: entity_id} = params
    maybe_entity = ElvenCaching.get_entity_by_id(entity_type, entity_id)

    case maybe_entity do
      {:ok, entity} -> EntityInteractions.set_dir(entity, dir)
      _ -> :ok
    end

    {:cont, socket}
  end

  @spec ncif(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def ncif("ncif", params, %Socket{} = socket) do
    %{entity_type: entity_type, entity_id: entity_id} = params
    %{character_id: character_id} = socket.assigns

    # Get current character
    {:ok, character} = CharacterRegistry.get(character_id)
    %EntityPosition{map_id: map_id} = MapEntity.position(character)

    # Get target
    maybe_entity = ElvenCaching.get_entity_by_id(entity_type, entity_id)

    # If the entity is on the same map
    case maybe_entity do
      {:ok, %{map_id: ^map_id} = target} ->
        Socket.send(socket, EntityViews.render(:st, target))

      _ ->
        :ok
    end

    {:cont, socket}
  end

  @spec walk(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def walk("walk", params, %Socket{} = socket) do
    %{
      pos_x: pos_x,
      pos_y: pos_y,
      speed: speed
    } = params

    %{character_id: character_id} = socket.assigns
    {:ok, character} = CharacterRegistry.get(character_id)

    # TODO: Later do some security checks (distance, checksum, dest cell, etc...)
    if speed != character.speed do
      raise "[walk] Invalid speed detected for #{character.name} (#{character_id})"
    end

    {:ok, _} = EntityInteractions.move(character, pos_x, pos_y)

    {:cont, socket}
  end

  @spec sit(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def sit("rest", params, %Socket{} = socket) do
    IO.inspect(params)
    {:cont, socket}
  end
end
